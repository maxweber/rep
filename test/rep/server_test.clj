(ns rep.server-test
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rep.evaluation :as evaluation]
            [rep.server :as server]))

(def token "test-token")

(defn with-system
  "Runs `f` with a started system in a folder of its own."
  [options f]
  (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
        w (server/start! (server/prepare (merge {:rep/dir folder
                                                 :rep/port 0
                                                 :rep/token token}
                                                options)))]
    (try
      (f w)
      (finally
        (server/stop! w)
        (fs/delete-tree folder)))))

(defn address
  [w & parts]
  (apply str "http://127.0.0.1:" (:rep/port w) parts))

(defn uuid
  []
  (str (random-uuid)))

(defn bash
  "The code that runs `script` with bash."
  [script]
  (str "(sh " (pr-str script) ")"))

(defn evaluate
  "POSTs `code`. Options: `:id`, `:as`."
  ([w session code]
   (evaluate w session code nil))
  ([w session code {:keys [id as]}]
   (http/post (address w "/rep/" session "/" (or id (uuid)))
              (cond-> {:headers {"Authorization" (str "Bearer " token)}
                       :body code
                       :throw false}
                as (assoc :as as)))))

(defn look
  "GETs the evaluation `id`, from byte `from` when given."
  ([w session id]
   (look w session id nil))
  ([w session id from]
   (http/get (address w "/rep/" session "/" id (when from (str "?from=" from)))
             {:headers {"Authorization" (str "Bearer " token)}
              :throw false})))

(defn outcome
  [response]
  (select-keys (:headers response) ["rep-status" "rep-exit" "rep-session" "rep-evaluation"]))

(deftest value-test
  (with-system {}
    (fn [w]
      (let [response (evaluate w (uuid) "(+ 1 2)")]
        (is (= 200 (:status response)))
        (is (= "3\n" (:body response)))
        (is (= {"rep-status" "done" "rep-exit" "0" "rep-session" "started" "rep-evaluation" "new"}
               (outcome response))))
      (testing "printed text comes before the value, nil is not printed"
        (is (= "hi\n:v\n" (:body (evaluate w (uuid) "(println \"hi\") :v"))))
        (is (= "raw \"text\"" (:body (evaluate w (uuid) "(print \"raw \\\"text\\\"\")"))))
        (is (= "" (:body (evaluate w (uuid) "nil"))))))))

(deftest session-test
  (with-system {}
    (fn [w]
      (let [session (uuid)]
        (is (= "#'user/a\n" (:body (evaluate w session "(def a 20)"))))
        (let [response (evaluate w session "(* 2 a)")]
          (is (= "40\n" (:body response)))
          (is (= "continued" (get (:headers response) "rep-session"))))
        (testing "another session does not see it"
          (let [response (evaluate w (uuid) "(* 2 a)")]
            (is (str/includes? (:body response) "Unable to resolve symbol: a"))
            (is (= {"rep-status" "failed" "rep-exit" "1"}
                   (select-keys (outcome response) ["rep-status" "rep-exit"])))))
        (testing "every request starts in user, other namespaces stay"
          (is (= "\"other\"\n" (:body (evaluate w session "(ns other) (def b 2) (str *ns*)"))))
          (is (= "[\"user\" 2]\n" (:body (evaluate w session "[(str *ns*) other/b]")))))))))

(deftest closed-context-test
  (with-system {}
    (fn [w]
      (doseq [code ["(java.io.File. \"x\")" "(System/exit 0)" "(slurp \"/etc/passwd\")" "(slurp \"../x\")"]]
        (is (= "failed" (get (:headers (evaluate w (uuid) code)) "rep-status")) code))
      (is (= "Error: Not a path inside the work folder: \"/etc/passwd\"\n"
             (:body (evaluate w (uuid) "(slurp \"/etc/passwd\")")))))))

(deftest error-test
  (with-system {}
    (fn [w]
      (let [response (evaluate w (uuid) "(println \"before\")\n(/ 1 0)")]
        (is (= "before\nError: Divide by zero\n" (:body response)))
        (is (= "failed" (get (:headers response) "rep-status"))))
      (is (= "Error: Unable to resolve symbol: foo (line 2, column 1)\n"
             (:body (evaluate w (uuid) "1\n(foo)"))))
      (is (str/starts-with? (:body (evaluate w (uuid) "(+ 1"))
                            "Error: EOF while reading"))
      (testing "the data of an exception the code throws is shown"
        (is (= "Error: mine\n{:a 1}\n"
               (:body (evaluate w (uuid) "(throw (ex-info \"mine\" {:a 1}))"))))))))

(deftest bash-test
  (with-system {}
    (fn [w]
      (testing "the output is what bash wrote, quotes and standard error included"
        (let [response (evaluate w (uuid) "(sh \"printf 'a \\\"quoted\\\" line\\\\n'; echo err >&2\")")]
          (is (= "a \"quoted\" line\nerr\n" (:body response)))
          (is (= "0" (get (:headers response) "rep-exit")))))
      (testing "the exit code of the last call becomes the evaluation's"
        (let [response (evaluate w (uuid) "(sh \"echo no; exit 3\")")]
          (is (= "no\n" (:body response)))
          (is (= {"rep-status" "done" "rep-exit" "3"}
                 (select-keys (outcome response) ["rep-status" "rep-exit"])))))
      (testing "the result is data for the code"
        (is (= "3\n" (:body (evaluate w (uuid) "(:exit (sh \"exit 3\"))")))))
      (testing "a script uploaded as a file runs without any quoting"
        (let [session (uuid)
              upload (edn/read-string (:body (evaluate w session "(upload-url \"scripts/run.sh\")")))]
          (is (= 201 (:status (http/put upload {:body "echo \"it's $((1+2)) \\\"quoted\\\"\"\nexit 4\n"
                                                :throw false}))))
          (let [response (evaluate w session "(sh \"bash scripts/run.sh\")")]
            (is (= "it's 3 \"quoted\"\n" (:body response)))
            (is (= "4" (get (:headers response) "rep-exit"))))))
      (testing "bash runs in the work folder, without the server's token"
        (is (= (str (fs/real-path (:rep/dir w)) "\n")
               (:body (evaluate w (uuid) (bash "pwd -P")))))
        (is (= "none\n"
               (:body (evaluate w (uuid) (bash "echo ${REP_TOKEN:-none}")))))))))

(deftest idempotency-test
  (with-system {}
    (fn [w]
      (let [session (uuid)
            id (uuid)
            code "(spit \"count.txt\" \"x\" :append true) (slurp \"count.txt\")"
            first-answer (evaluate w session code {:id id})
            second-answer (evaluate w session code {:id id})]
        (is (= "\"x\"\n" (:body first-answer) (:body second-answer)))
        (is (= "new" (get (:headers first-answer) "rep-evaluation")))
        (is (= "replayed" (get (:headers second-answer) "rep-evaluation")))
        (is (= "x" (slurp (fs/file (:rep/dir w) "count.txt"))) "the code ran once")
        (testing "the same id with other code is refused"
          (let [response (evaluate w session "(+ 1 1)" {:id id})]
            (is (= 409 (:status response)))
            (is (str/includes? (:body response) "was used for another request"))))
        (testing "a GET answers the text again, or the rest of it"
          (is (= "\"x\"\n" (:body (look w session id))))
          (is (= "\"\n" (:body (look w session id 2))))
          (is (= 404 (:status (look w session (uuid))))))))))

(deftest streaming-test
  (with-system {}
    (fn [w]
      (let [session (uuid)
            id (uuid)
            started (System/currentTimeMillis)
            response (evaluate w session (bash "echo one; sleep 1; echo two") {:id id :as :stream})
            in (:body response)
            buffer (byte-array 100)
            first-chunk (String. buffer 0 (.read in buffer) "UTF-8")
            first-after (- (System/currentTimeMillis) started)
            remaining (slurp in)]
        (is (= "running" (get (:headers response) "rep-status")))
        (is (= "one\n" first-chunk))
        (is (< first-after 800) "the first line comes before bash is done")
        (is (= "two\n" remaining))
        (testing "the outcome is in the headers of a GET afterwards"
          (let [after (look w session id 4)]
            (is (= "two\n" (:body after)))
            (is (= {"rep-status" "done" "rep-exit" "0"} (outcome after)))))))))

(deftest busy-and-stop-test
  (with-system {}
    (fn [w]
      (let [session (uuid)
            id (uuid)
            long-running (future (evaluate w session (bash "echo started; sleep 30") {:id id}))]
        (Thread/sleep 300)
        (testing "a session evaluates one request at a time"
          (let [response (evaluate w session "(+ 1 2)")]
            (is (= 409 (:status response)))
            (is (str/includes? (:body response) id))))
        (testing "another session is free"
          (is (= "3\n" (:body (evaluate w (uuid) "(+ 1 2)")))))
        (testing "DELETE stops it and the session is free again"
          (let [started (System/currentTimeMillis)
                response (http/delete (address w "/rep/" session "/" id)
                                      {:headers {"Authorization" (str "Bearer " token)}
                                       :throw false})]
            (is (= 200 (:status response)))
            (is (< (- (System/currentTimeMillis) started) 3000)))
          (is (= "started\n" (:body (deref long-running 3000 nil))))
          (is (= {"rep-status" "stopped" "rep-exit" "130"} (outcome (look w session id))))
          (is (= "3\n" (:body (evaluate w session "(+ 1 2)")))))))))

(deftest files-test
  (with-system {}
    (fn [w]
      (let [session (uuid)
            read-url (fn [code]
                       (edn/read-string (:body (evaluate w session code))))
            upload (read-url "(upload-url \"in/clip one+two.txt\")")
            stored (http/put upload {:body "the footage" :throw false})]
        (is (str/includes? upload "/file/in/clip%20one%2Btwo.txt?expires="))
        (is (= 201 (:status stored)))
        (is (= "the footage" (slurp (fs/file (:rep/dir w) "in" "clip one+two.txt"))))
        (is (= "\"the footage\"\n" (:body (evaluate w session "(slurp \"in/clip one+two.txt\")"))))
        (testing "a download URL answers the file without a token"
          (let [download (read-url "(download-url \"in/clip one+two.txt\")")]
            (is (= "the footage" (:body (http/get download {:throw false}))))
            (testing "but not another method, path or signature"
              (is (= 403 (:status (http/put download {:body "x" :throw false}))))
              (is (= 403 (:status (http/get (str/replace download "one%2Btwo" "three") {:throw false}))))
              (is (= 403 (:status (http/get (str download "x") {:throw false})))))))
        (testing "an expired URL is refused"
          (is (= 403 (:status (http/get (read-url "(download-url \"in/clip one+two.txt\" -1)")
                                        {:throw false})))))
        (testing "no URL for a path outside the work folder or into .rep"
          (is (= "failed" (get (:headers (evaluate w session "(upload-url \"../x\")")) "rep-status")))
          (is (= "failed" (get (:headers (evaluate w session "(download-url \".rep/sessions\")")) "rep-status"))))
        (testing "a missing file"
          (is (= 404 (:status (http/get (read-url "(download-url \"nothing.txt\")") {:throw false})))))))))

(deftest access-test
  (with-system {}
    (fn [w]
      (let [path (str "/rep/" (uuid) "/" (uuid))]
        (is (= 401 (:status (http/post (address w path) {:body "1" :throw false}))))
        (is (= 401 (:status (http/post (address w path) {:headers {"Authorization" "Bearer wrong"}
                                                         :body "1"
                                                         :throw false}))))
        (is (= 400 (:status (http/post (address w "/rep/not-a-uuid/" (uuid))
                                       {:headers {"Authorization" (str "Bearer " token)}
                                        :body "1"
                                        :throw false}))))
        (is (= 400 (:status (evaluate w (uuid) ""))))
        (let [usage (http/get (address w "/") {:throw false})]
          (is (= 200 (:status usage)))
          (is (str/includes? (:body usage) "POST   /rep/")))))))

(deftest restart-test
  (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
        options {:rep/dir folder :rep/port 0 :rep/token token}
        session (uuid)
        finished (uuid)
        cut-off (uuid)
        before (server/start! (server/prepare options))]
    (try
      (is (= "#'user/a\n" (:body (evaluate before session "(def a 1)" {:id finished}))))
      (future (evaluate before session (bash "echo begun; sleep 30") {:id cut-off}))
      (Thread/sleep 300)
      (server/stop! before)
      (let [after (server/start! (server/prepare options))]
        (try
          (testing "an answer of before the restart is still there"
            (let [response (evaluate after session "(def a 1)" {:id finished})]
              (is (= "#'user/a\n" (:body response)))
              (is (= "replayed" (get (:headers response) "rep-evaluation")))))
          (testing "what ran during the restart is told as interrupted"
            (let [response (look after session cut-off)]
              (is (= "begun\n" (:body response)))
              (is (= "interrupted" (get (:headers response) "rep-status")))))
          (testing "the session says that its definitions are gone"
            (let [response (evaluate after session "a")]
              (is (= "restarted" (get (:headers response) "rep-session")))
              (is (str/includes? (:body response) "Unable to resolve symbol: a"))))
          (finally
            (server/stop! after))))
      (finally
        (evaluation/stop! before session cut-off)
        (fs/delete-tree folder)))))

(deftest output-limit-test
  (with-system {:rep/output-limit 1000}
    (fn [w]
      (let [response (evaluate w (uuid) (bash "yes | head -c 100000"))]
        (is (= "failed" (get (:headers response) "rep-status")))
        (is (< (count (:body response)) 1200))
        (is (str/includes? (:body response) "The output passed its limit of 1000 bytes."))))))

(deftest help-test
  (with-system {:rep/namespaces {'shop {'price (fn [amount] (* 2 amount))}}}
    (fn [w]
      (let [help (:body (evaluate w (uuid) "(help)"))]
        (is (str/starts-with? help "(sh script)\n"))
        (is (str/includes? help "Namespaces added here: shop."))
        (is (not (str/ends-with? help "nil\n"))))
      (is (str/includes? (:body (evaluate w (uuid) "(clojure.repl/doc sh)"))
                         "Runs the bash script in the work folder."))
      (testing "the host's own functions"
        (is (= "42\n" (:body (evaluate w (uuid) "(shop/price 21)"))))))))
