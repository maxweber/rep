(ns rep.server-test
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [org.httpkit.server :as http-server]
            [rep.server :as server]
            [rep.session :as session]))

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

(defn post
  "One POST of `code`. Options: `:id`, `:from`, `:as`."
  [w session code {:keys [id from as]}]
  (http/post (address w "/rep/" session "/" id (when from (str "?from=" from)))
             (cond-> {:headers {"Authorization" (str "Bearer " token)}
                      :body code
                      :throw false}
               as (assoc :as as))))

(defn evaluate
  "POSTs `code` and follows it to its end, the way the client does: an
   answer that says running is asked for again from where it stopped.
   The answer has the whole text and the final headers."
  ([w session code]
   (evaluate w session code nil))
  ([w session code {:keys [id]}]
   (let [id (or id (uuid))
         first-answer (post w session code {:id id})]
     (loop [answer first-answer
            body (:body first-answer)]
       (if (= "running" (get (:headers answer) "rep-status"))
         (let [answer (post w session code {:id id :from (count (.getBytes body "UTF-8"))})]
           (recur answer (str body (:body answer))))
         (-> first-answer
             (assoc :body body)
             (update :headers merge (select-keys (:headers answer) ["rep-status" "rep-exit"]))))))))

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

(deftest babashka-test
  (with-system {}
    (fn [w]
      (testing "a session is babashka, with its namespaces and the work folder as its place"
        (is (= "\"x\"\n"
               (:body (evaluate w (uuid) (str "(require '[babashka.fs :as fs])"
                                              "(fs/create-dirs \"notes\")"
                                              "(spit \"notes/a.txt\" \"x\")"
                                              "(slurp \"notes/a.txt\")")))))
        (is (= "x" (slurp (fs/file (:rep/dir w) "notes" "a.txt")))))
      (testing "it does not get the server's token"
        (is (= {"PATH" "/bin"}
               (session/environment {"PATH" "/bin" "REP_TOKEN" "t" "REP_SECRET" "s"})))))))

(deftest error-test
  (with-system {}
    (fn [w]
      (testing "babashka's own error text, and the request ends at the first error"
        (let [response (evaluate w (uuid) "(println \"before\")\n(/ 1 0)\n(println \"after\")")]
          (is (= "before\njava.lang.ArithmeticException: Divide by zero [at NO_SOURCE_PATH:2:1]\n"
                 (:body response)))
          (is (= {"rep-status" "failed" "rep-exit" "1"}
                 (select-keys (outcome response) ["rep-status" "rep-exit"])))))
      (is (str/includes? (:body (evaluate w (uuid) "1\n(foo)"))
                         "Unable to resolve symbol: foo"))
      (is (str/includes? (:body (evaluate w (uuid) "(+ 1"))
                         "EOF while reading"))
      (testing "the data of an exception the code throws is shown"
        (is (str/ends-with? (:body (evaluate w (uuid) "(throw (ex-info \"mine\" {:a 1}))"))
                            "mine [at NO_SOURCE_PATH:1:1]\n{:a 1}\n"))))))

(deftest bash-test
  (with-system {}
    (fn [w]
      (testing "the output is what bash wrote, quotes and standard error included"
        (is (= "a \"quoted\" line\nerr\n{:exit 0}\n"
               (:body (evaluate w (uuid) "(sh \"printf 'a \\\"quoted\\\" line\\\\n'; echo err >&2\")")))))
      (testing "a failing script is a value, the request is done all the same"
        (let [response (evaluate w (uuid) "(sh \"echo no; exit 3\")")]
          (is (= "no\n{:exit 3}\n" (:body response)))
          (is (= {"rep-status" "done" "rep-exit" "0"}
                 (select-keys (outcome response) ["rep-status" "rep-exit"]))))
        (is (= "3\n" (:body (evaluate w (uuid) "(:exit (sh \"exit 3\"))")))))
      (testing "with-out-str keeps the output as a string"
        (is (= "\"captured\\n\"\n" (:body (evaluate w (uuid) "(with-out-str (sh \"echo captured\"))")))))
      (testing "a script uploaded as a file runs without any quoting"
        (let [session (uuid)
              upload (edn/read-string (:body (evaluate w session "(upload-url \"scripts/run.sh\")")))]
          (is (= 201 (:status (http/put upload {:body "echo \"it's $((1+2)) \\\"quoted\\\"\"\nexit 4\n"
                                                :throw false}))))
          (is (= "it's 3 \"quoted\"\n{:exit 4}\n"
                 (:body (evaluate w session "(sh \"bash scripts/run.sh\")"))))))
      (testing "bash runs in the work folder"
        (is (= (str (fs/real-path (:rep/dir w)) "\n{:exit 0}\n")
               (:body (evaluate w (uuid) (bash "pwd -P")))))))))

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
          (let [response (post w session "(+ 1 1)" {:id id})]
            (is (= 409 (:status response)))
            (is (str/includes? (:body response) "was used for another request"))))
        (testing "from answers the rest of the text"
          (let [response (post w session code {:id id :from 2})]
            (is (= "\"\n" (:body response)))
            (is (= {"rep-status" "done" "rep-exit" "0" "rep-evaluation" "replayed"}
                   (outcome response)))))))))

(deftest streaming-test
  (with-system {:rep/grace-ms 100}
    (fn [w]
      (let [session (uuid)
            _ (evaluate w session ":warm")
            id (uuid)
            code (bash "echo one; sleep 1; echo two")
            started (System/currentTimeMillis)
            response (post w session code {:id id :as :stream})
            in (:body response)
            buffer (byte-array 100)
            first-chunk (String. buffer 0 (.read in buffer) "UTF-8")
            first-after (- (System/currentTimeMillis) started)
            remaining (slurp in)]
        (is (= "running" (get (:headers response) "rep-status")))
        (is (= "one\n" first-chunk))
        (is (< first-after 800) "the first line comes before bash is done")
        (is (= "two\n{:exit 0}\n" remaining))
        (testing "the same POST again tells the outcome and the rest, here nothing"
          (let [after (post w session code {:id id :from (count (str first-chunk remaining))})]
            (is (= "" (:body after)))
            (is (= {"rep-status" "done" "rep-exit" "0" "rep-evaluation" "replayed"}
                   (outcome after)))))))))

(deftest second-request-test
  (with-system {}
    (fn [w]
      (let [session (uuid)
            hung (future (evaluate w session (bash "echo started; sleep 987.5")))]
        (Thread/sleep 800)
        (testing "a session answers a second request while the first runs"
          (is (= "3\n" (:body (evaluate w session "(+ 1 2)")))))
        (testing "which can end a hung script"
          (evaluate w session (bash "pkill -f '[s]leep 987.5'"))
          (is (= "started\n{:exit 143}\n" (:body (deref hung 5000 nil)))))))))

(deftest future-test
  (with-system {}
    (fn [w]
      (let [session (uuid)]
        (testing "long work goes into a future, its result into an atom"
          (is (= "false\n"
                 (:body (evaluate w session (str "(def result (atom nil))"
                                                 "(def job (future (reset! result (with-out-str (sh \"sleep 0.5; echo rendered\")))))"
                                                 "(realized? job)")))))
          (Thread/sleep 1000)
          (is (= "[true \"rendered\\n\"]\n" (:body (evaluate w session "[(realized? job) @result]")))))
        (testing "a future without with-out-str works too, its output is dropped"
          (evaluate w session "(def plain (future (sh \"sleep 0.3; echo dropped\")))")
          (Thread/sleep 800)
          (is (= "{:exit 0}\n" (:body (evaluate w session "@plain")))))
        (testing "a later request can cancel it, and the script it runs ends too"
          (evaluate w session "(def slow (future (sh \"sleep 987.6\")))")
          (Thread/sleep 300)
          (is (= "[true true]\n"
                 (:body (evaluate w session "[(future-cancel slow) (future-cancelled? slow)]"))))
          (Thread/sleep 300)
          (is (= "0\n{:exit 0}\n"
                 (:body (evaluate w session (bash "pgrep -f '[s]leep 987.6' | wc -l"))))))))))

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
      (let [path (str "/rep/" (uuid) "/" (uuid))
            authorized {"Authorization" (str "Bearer " token)}]
        (is (= 401 (:status (http/post (address w path) {:body "1" :throw false}))))
        (is (= 401 (:status (http/post (address w path) {:headers {"Authorization" "Bearer wrong"}
                                                         :body "1"
                                                         :throw false}))))
        (is (= 400 (:status (http/post (address w "/rep/not-a-uuid/" (uuid))
                                       {:headers authorized :body "1" :throw false}))))
        (is (= 400 (:status (http/post (address w path) {:headers authorized :body "" :throw false}))))
        (testing "POST is the only way in"
          (is (= 405 (:status (http/get (address w path) {:headers authorized :throw false}))))
          (is (= 405 (:status (http/delete (address w path) {:headers authorized :throw false})))))
        (let [usage (http/get (address w "/") {:throw false})]
          (is (= 200 (:status usage)))
          (is (str/includes? (:body usage) "POST /rep/")))))))

(deftest restart-test
  (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
        options {:rep/dir folder :rep/port 0 :rep/token token}
        session (uuid)
        finished (uuid)
        cut-off (uuid)
        long-code (bash "echo begun; sleep 30")
        before (server/start! (server/prepare options))]
    (try
      (is (= "#'user/a\n" (:body (evaluate before session "(def a 1)" {:id finished}))))
      (future (post before session long-code {:id cut-off}))
      (Thread/sleep 1500)
      (http-server/server-stop! (:rep/server before))
      (let [after (server/start! (server/prepare options))]
        (try
          (testing "an answer of before the restart is still there"
            (let [response (evaluate after session "(def a 1)" {:id finished})]
              (is (= "#'user/a\n" (:body response)))
              (is (= "replayed" (get (:headers response) "rep-evaluation")))))
          (testing "what ran during the restart is told as interrupted"
            (let [response (post after session long-code {:id cut-off})]
              (is (= "begun\n" (:body response)))
              (is (= "interrupted" (get (:headers response) "rep-status")))
              (is (nil? (get (:headers response) "rep-exit")))))
          (testing "the session says that its definitions are gone"
            (let [response (evaluate after session "a")]
              (is (= "restarted" (get (:headers response) "rep-session")))
              (is (str/includes? (:body response) "Unable to resolve symbol: a"))))
          (finally
            (server/stop! after))))
      (finally
        (server/stop! before)
        (fs/delete-tree folder)))))

(deftest process-end-test
  (with-system {}
    (fn [w]
      (let [session (uuid)]
        (is (= "#'user/a\n" (:body (evaluate w session "(def a 1)"))))
        (testing "code can end its session, the request then fails and says why"
          (let [response (evaluate w session "(System/exit 0)")]
            (is (= "The session's process ended. Its definitions are gone.\n" (:body response)))
            (is (= {"rep-status" "failed" "rep-exit" "1"}
                   (select-keys (outcome response) ["rep-status" "rep-exit"])))))
        (testing "the next request gets a new process"
          (let [response (evaluate w session "(+ 1 2)")]
            (is (= "3\n" (:body response)))
            (is (= "restarted" (get (:headers response) "rep-session")))))))))

(deftest output-limit-test
  (with-system {:rep/output-limit 1000}
    (fn [w]
      (let [session (uuid)
            response (evaluate w session (bash "yes | head -c 100000"))]
        (is (= "failed" (get (:headers response) "rep-status")))
        (is (< (count (:body response)) 1200))
        (is (str/includes? (:body response) "The output passed its limit of 1000 bytes."))
        (testing "the session goes on"
          (is (= "3\n" (:body (evaluate w session "(+ 1 2)")))))))))

(deftest help-test
  (with-system {}
    (fn [w]
      (let [help (:body (evaluate w (uuid) "(help)"))]
        (is (str/starts-with? help "This is babashka."))
        (is (str/includes? help "(sh script)"))
        (is (not (str/ends-with? help "nil\n")))))))

(deftest prelude-test
  (let [prelude (str (fs/create-temp-file {:prefix "rep-prelude" :suffix ".clj"}))]
    (try
      (testing "a host adds its own functions with a file every session loads"
        (spit prelude "(ns shop)\n(defn price [amount] (* 2 amount))\n")
        (with-system {:rep/prelude prelude}
          (fn [w]
            (is (= "42\n" (:body (evaluate w (uuid) "(shop/price 21)")))))))
      (testing "a prelude that does not load fails the request with the reason"
        (spit prelude "(ns shop)\n(defn price [amount] (unknown amount))\n")
        (with-system {:rep/prelude prelude}
          (fn [w]
            (let [response (evaluate w (uuid) "(+ 1 2)")]
              (is (= "failed" (get (:headers response) "rep-status")))
              (is (str/starts-with? (:body response) "The session's prelude failed:"))))))
      (finally
        (fs/delete prelude)))))
