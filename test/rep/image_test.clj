(ns rep.image-test
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [org.httpkit.server :as http-server]
            [rep.image :as image]
            [rep.server :as server]
            [rep.server-test :as server-test :refer [evaluate outcome uuid with-system]]))

(defn with-image
  "Runs `f` with a started system whose sessions share one image."
  [options f]
  (with-system (merge {:rep/shared-image? true} options) f))

(defn wait-for
  "Asks `ready?` every few milliseconds until it answers something
   truthy, for `timeout-ms` at most. Answers that, else nil."
  [timeout-ms ready?]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (ready?)
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 10)
            (recur))))))

(defn running
  "Waits until the image runs in `generation`, without a request that
   would ask for it. Answers its status, or nil when that takes too long."
  [w generation]
  (wait-for 10000 #(let [status (image/status w)]
                     (when (and (= :running (:image/state status))
                                (= generation (:image/generation status)))
                       status))))

(defn alive?
  [pid]
  (boolean (some-> (java.lang.ProcessHandle/of pid)
                   (.orElse nil)
                   (.isAlive))))

(defn nrepl-children
  "The process ids of the babashkas with an nREPL server that this
   process started and that still run."
  []
  (->> (.descendants (java.lang.ProcessHandle/current))
       (.iterator)
       (iterator-seq)
       (filter #(.isAlive %))
       (filter #(some-> (.info %)
                        (.commandLine)
                        (.orElse nil)
                        (str/includes? "nrepl-server")))
       (mapv #(.pid %))))

(defn session-header
  [response]
  (get (:headers response) "rep-session"))

(defn prelude-file
  "A new prelude file with `code` in it."
  [code]
  (let [file (str (fs/create-temp-file {:prefix "rep-prelude" :suffix ".clj"}))]
    (spit file code)
    file))

(deftest shared-definitions-test
  (with-image {}
    (fn [w]
      (let [one (uuid)
            two (uuid)
            pid (:image/pid (image/status w))]
        (testing "a definition made under one session uuid is there under another"
          (let [response (evaluate w one "(def a 20)")]
            (is (= "#'user/a\n" (:body response)))
            (is (= "started" (session-header response))))
          (let [response (evaluate w two "(* 2 a)")]
            (is (= "40\n" (:body response)))
            (is (= "started" (session-header response)) "the uuid is new, the image is not")))
        (testing "a function that one caller redefines is redefined for all"
          (evaluate w one "(defn price [] 1)")
          (evaluate w two "(defn price [] 2)")
          (let [response (evaluate w one "(price)")]
            (is (= "2\n" (:body response)))
            (is (= "continued" (session-header response)))))
        (testing "all of them evaluate in the one process of the image"
          (is (= (str pid "\n")
                 (:body (evaluate w one "(.pid (java.lang.ProcessHandle/current))"))
                 (:body (evaluate w (uuid) "(.pid (java.lang.ProcessHandle/current))"))))
          (is (= [pid] (nrepl-children))))
        (testing "every request still starts in user"
          (is (= "\"other\"\n" (:body (evaluate w one "(ns other) (def b 2) (str *ns*)"))))
          (is (= "[\"user\" 2]\n" (:body (evaluate w two "[(str *ns*) other/b]")))))
        (testing "two evaluations run at the same time, each on its own connection"
          (let [hung (future (evaluate w one (server-test/bash "echo started; sleep 986.3")))]
            (Thread/sleep 800)
            (is (= "3\n" (:body (evaluate w two "(+ 1 2)"))))
            (evaluate w two (server-test/bash "pkill -f '[s]leep 986.3'"))
            (is (= "started\n{:exit 143}\n" (:body (deref hung 5000 nil))))))
        (testing "the usage text and help tell a caller that the image is shared"
          (is (str/includes? (:body (http/get (server-test/address w "/"))) "one shared image"))
          (is (str/includes? (:body (evaluate w one "(help)")) "All callers share this one process")))))))

(deftest default-mode-test
  (with-system {}
    (fn [w]
      (testing "without the option a session has its own process, and nothing mentions an image"
        (is (= :stopped (:image/state (image/status w))))
        (is (not (str/includes? (:body (http/get (server-test/address w "/"))) "shared image")))
        (is (not (str/includes? (:body (evaluate w (uuid) "(help)")) "share"))))
      (testing "evaluate and start! are for the shared image only, and say so"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"rep.image/evaluate needs the shared image"
                              (image/evaluate w "(+ 1 2)")))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"rep.image/start! needs the shared image"
                              (image/start! w)))))))

(deftest eager-start-test
  (let [prelude (prelude-file (str "(ns shop)\n"
                                   "(spit \"loads.txt\" \"load\\n\" :append true)\n"
                                   "(defn price [amount] (* 2 amount))\n"))]
    (try
      (with-image {:rep/prelude prelude}
        (fn [w]
          (let [loads #(slurp (fs/file (:rep/dir w) "loads.txt"))]
            (testing "the image runs when start! comes back, before any request"
              (let [status (image/status w)]
                (is (= :running (:image/state status)))
                (is (= 1 (:image/generation status)))
                (is (alive? (:image/pid status)))
                (is (pos? (:image/port status)))
                (is (= [(:image/pid status)] (nrepl-children))))
              (is (= "load\n" (loads)) "and its setup code has run"))
            (testing "the setup code runs once for the image, not once for a session"
              (is (= "42\n" (:body (evaluate w (uuid) "(shop/price 21)"))))
              (is (= "42\n" (:body (evaluate w (uuid) "(shop/price 21)"))))
              (is (= "load\n" (loads)))
              (is (= 1 (:image/generation (image/status w))))))))
      (finally
        (fs/delete prelude)))))

(deftest exit-test
  (with-image {}
    (fn [w]
      (let [before (uuid)
            first-pid (:image/pid (image/status w))]
        (is (= "#'user/a\n" (:body (evaluate w before "(def a 1)"))))
        (testing "code can end the image, its request fails like one in a session's own process"
          (let [response (evaluate w before "(System/exit 0)")]
            (is (= "The session's process ended. Its definitions are gone.\n" (:body response)))
            (is (= {"rep-status" "failed" "rep-exit" "1"}
                   (select-keys (outcome response) ["rep-status" "rep-exit"])))))
        (testing "the next image starts by itself within about a second, nobody asks for it"
          (let [ended (System/currentTimeMillis)
                status (running w 2)]
            (is (some? status))
            (is (< (- (System/currentTimeMillis) ended) 2000))
            (is (not= first-pid (:image/pid status)))
            (is (not (alive? first-pid)))
            (is (= [(:image/pid status)] (nrepl-children)))))
        (testing "a uuid that evaluated before the restart learns that its definitions are gone"
          (let [response (evaluate w before "a")]
            (is (= "restarted" (session-header response)))
            (is (str/includes? (:body response) "Unable to resolve symbol: a")))
          (is (= "continued" (session-header (evaluate w before "1")))))
        (testing "a uuid that shows up after the restart is started"
          (is (= "started" (session-header (evaluate w (uuid) "1")))))))))

(deftest kill-test
  (with-image {:rep/grace-ms 100}
    (fn [w]
      (let [before (uuid)
            first-pid (:image/pid (image/status w))
            _ (evaluate w before "(def a 1)")
            session (uuid)
            id (uuid)
            output (fs/file (:rep/state-dir w) "sessions" session id "output")
            cut-off (future (evaluate w session "(println \"begun\") (Thread/sleep 30000)" {:id id}))]
        (is (wait-for 5000 #(and (fs/exists? output) (= "begun\n" (slurp output)))))
        (process/shell "kill" "-9" (str first-pid))
        (is (wait-for 2000 #(not (alive? first-pid))))
        (testing "a request that comes while the next image starts waits for it"
          (let [response (evaluate w (uuid) "(+ 1 2)")]
            (is (= "3\n" (:body response)))
            (is (= "started" (session-header response)))))
        (testing "an evaluation that ran when the image died fails and says why"
          (let [response (deref cut-off 5000 nil)]
            (is (= "begun\nThe session's process ended. Its definitions are gone.\n" (:body response)))
            (is (= {"rep-status" "failed" "rep-exit" "1"}
                   (select-keys (outcome response) ["rep-status" "rep-exit"])))))
        (testing "the image that was killed from outside has a successor"
          (let [status (running w 2)]
            (is (some? status))
            (is (not= first-pid (:image/pid status)))
            (is (= [(:image/pid status)] (nrepl-children)))))
        (testing "a uuid from before the kill is told restarted, once"
          (is (= "restarted" (session-header (evaluate w before "1"))))
          (is (= "continued" (session-header (evaluate w before "1")))))))))

(deftest server-restart-test
  (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
        options {:rep/dir folder :rep/port 0 :rep/token server-test/token :rep/shared-image? true}
        session (uuid)
        before (server/start! (server/prepare options))]
    (try
      (is (= "started" (session-header (evaluate before session "(def a 1)"))))
      (server/stop! before)
      (let [after (server/start! (server/prepare options))]
        (try
          (testing "a uuid of an earlier run of the server is told that its definitions are gone"
            (is (= "restarted" (session-header (evaluate after session "1"))))
            (is (= "started" (session-header (evaluate after (uuid) "1")))))
          (finally
            (server/stop! after))))
      (finally
        (server/stop! before)
        (fs/delete-tree folder)))))

(deftest backoff-test
  (testing "the pauses double from the first one up to the longest"
    (is (= [0 100 200 400 800 1600 3200 5000 5000]
           (map #(image/pause-ms {:rep/backoff-ms 100 :rep/backoff-max-ms 5000} %) (range 9)))))
  (let [prelude (prelude-file (str "(spit \"loads.txt\" \"load\\n\" :append true)\n"
                                   "(throw (ex-info \"broken prelude\" {}))\n"))
        starts (atom [])
        begun (System/currentTimeMillis)]
    (try
      (with-image {:rep/prelude prelude
                   :rep/on-image-start (fn [_w image] (swap! starts conj image))}
        (fn [w]
          (let [loads #(count (str/split-lines (slurp (fs/file (:rep/dir w) "loads.txt"))))]
            (testing "the server starts all the same, and whoever asks hears the reason"
              (let [status (image/status w)]
                (is (= :failed (:image/state status)))
                (is (str/includes? (:image/error status) "broken prelude")))
              (let [response (evaluate w (uuid) "(+ 1 2)")]
                (is (= "failed" (get (:headers response) "rep-status")))
                (is (str/starts-with? (:body response) "The session's prelude failed:")))
              (is (str/includes? (:error (image/evaluate w "(+ 1 2)")) "broken prelude")))
            (testing "the supervisor tries again and again, but it does not spin"
              (Thread/sleep (max 0 (- (+ begun 3000) (System/currentTimeMillis))))
              (is (<= 2 (loads) 5)
                  "pauses of 100, 200, 400, 800 and 1600 ms let five starts at most into three seconds")
              (is (<= 2 (:image/failures (image/status w))))
              (is (wait-for 2000 #(empty? (nrepl-children))) "a start that failed leaves no process")
              (is (empty? @starts)))
            (testing "once the prelude loads, the image runs without anybody asking"
              (spit prelude "(ns shop)\n(defn price [amount] (* 2 amount))\n")
              (is (some? (running w 1)))
              (is (= "42\n" (:body (evaluate w (uuid) "(shop/price 21)"))))
              (is (= [1] (map :image/generation @starts)))))))
      (finally
        (fs/delete prelude)))))

(deftest evaluate-test
  (with-image {}
    (fn [w]
      (testing "the host evaluates in the image without HTTP"
        (is (= {:out "" :value "3" :error nil} (image/evaluate w "(+ 1 2)")))
        (is (= {:out "hi\n" :value ":v" :error nil} (image/evaluate w "(println \"hi\") :v")))
        (is (= {:out "" :value "nil" :error nil} (image/evaluate w "nil")))
        (is (= {:out "" :value "{:a [1 \"two\"]}" :error nil} (image/evaluate w "{:a [1 \"two\"]}"))))
      (testing "an error ends the code at its first failing form and comes back as text"
        (is (= {:out "before\n"
                :value nil
                :error "java.lang.ArithmeticException: Divide by zero [at NO_SOURCE_PATH:2:1]"}
               (image/evaluate w "(println \"before\")\n(/ 1 0)\n(println \"after\")"))))
      (testing "what the code writes to standard error is output, babashka's error text is not"
        (is (= {:out "warn\n" :value "1" :error nil}
               (image/evaluate w "(binding [*out* *err*] (println \"warn\")) 1")))
        (let [answer (image/evaluate w "(binding [*out* *err*] (println \"warn\")) (throw (ex-info \"mine\" {}))")]
          (is (= "warn\n" (:out answer)))
          (is (str/includes? (:error answer) "mine"))))
      (testing "it is the same image the sessions evaluate in"
        (evaluate w (uuid) "(def from-http 5)")
        (is (= "5" (:value (image/evaluate w "from-http"))))
        (image/evaluate w "(def from-host 7)")
        (is (= "7\n" (:body (evaluate w (uuid) "from-host")))))
      (testing "every call starts in user"
        (is (= "\"elsewhere\"" (:value (image/evaluate w "(ns elsewhere) (str *ns*)"))))
        (is (= "\"user\"" (:value (image/evaluate w "(str *ns*)")))))
      (testing "nothing of it is kept on disk"
        (is (not (fs/exists? (fs/path (:rep/state-dir w) "sessions" "image")))))
      (testing "calls run side by side"
        (let [begun (System/currentTimeMillis)
              values (->> (range 8)
                          (mapv #(future (:value (image/evaluate w (str "(Thread/sleep 300) " %)))))
                          (mapv deref))]
          (is (= (map str (range 8)) values))
          (is (< (- (System/currentTimeMillis) begun) 1500))))
      (testing "a call that takes too long comes back at the timeout, and its code is interrupted"
        (let [begun (System/currentTimeMillis)
              answer (image/evaluate w "(println \"begun\") (Thread/sleep 1500) (spit \"late.txt\" \"x\")"
                                     {:timeout-ms 300})]
          (is (= {:out "begun\n" :value nil :error "The image did not answer within 300 ms."} answer))
          (is (< (- (System/currentTimeMillis) begun) 1000))
          (is (= "3" (:value (image/evaluate w "(+ 1 2)"))) "the next call works")
          (Thread/sleep 1800)
          (is (not (fs/exists? (fs/path (:rep/dir w) "late.txt"))))))
      (testing "a call that ends the image says so, the next one runs in the new image"
        (is (= {:out "" :value nil :error "The image's process ended."}
               (image/evaluate w "(System/exit 0)")))
        (is (= {:out "" :value "3" :error nil} (image/evaluate w "(+ 1 2)")))
        (is (= 2 (:image/generation (image/status w))))))))

(deftest on-image-start-test
  (let [calls (atom [])]
    (with-image {:rep/on-image-start (fn [w image]
                                       (swap! calls conj {:image image
                                                          :port (:rep/port w)
                                                          :value (:value (image/evaluate w "(+ 1 2)"))}))}
      (fn [w]
        (testing "the hook has run when start! comes back, with the system and the image"
          (is (= 1 (count @calls)))
          (let [{:keys [image port value]} (first @calls)
                status (image/status w)]
            (is (= (dissoc status :image/state) image))
            (is (= 1 (:image/generation image)))
            (is (= (:rep/port w) port))
            (is (= "3" value) "the image is ready for the hook to evaluate in")))
        (testing "it runs again after every restart"
          (evaluate w (uuid) "(System/exit 0)")
          (is (wait-for 5000 #(= 2 (count @calls))))
          (is (= [1 2] (map (comp :image/generation :image) @calls)))
          (is (apply not= (map (comp :image/pid :image) @calls)))))))
  (testing "a hook that throws breaks neither the start nor the supervision"
    (with-image {:rep/on-image-start (fn [_w _image] (throw (ex-info "broken hook" {})))}
      (fn [w]
        (is (= :running (:image/state (image/status w))))
        (is (= "3\n" (:body (evaluate w (uuid) "(+ 1 2)"))))
        (evaluate w (uuid) "(System/exit 0)")
        (is (some? (running w 2)))
        (is (= "3\n" (:body (evaluate w (uuid) "(+ 1 2)"))))))))

(deftest after-evaluation-test
  (doseq [shared? [false true]]
    (testing (if shared? "with the shared image:" "with a process per session:")
      (let [calls (atom [])]
        (with-system {:rep/shared-image? shared?
                      :rep/after-evaluation (fn [w evaluation]
                                              (swap! calls conj (assoc evaluation :port (:rep/port w))))}
          (fn [w]
            (let [session (uuid)
                  done (uuid)
                  failed (uuid)]
              (evaluate w session "(+ 1 2)" {:id done})
              (evaluate w session "(/ 1 0)" {:id failed})
              (testing "the hook hears about every evaluation of a POST, with its outcome"
                (is (wait-for 2000 #(= 2 (count @calls))))
                (is (= #{{:evaluation/session session
                          :evaluation/id done
                          :evaluation/state :done
                          :evaluation/exit 0
                          :evaluation/code "(+ 1 2)"
                          :port (:rep/port w)}
                         {:evaluation/session session
                          :evaluation/id failed
                          :evaluation/state :failed
                          :evaluation/exit 1
                          :evaluation/code "(/ 1 0)"
                          :port (:rep/port w)}}
                       (set @calls))))
              (testing "an answer that is replayed ran nothing, so the hook hears nothing"
                (evaluate w session "(+ 1 2)" {:id done})
                (Thread/sleep 200)
                (is (= 2 (count @calls)))))))
        (testing "a hook that throws does not break the request or the ones after it"
          (with-system {:rep/shared-image? shared?
                        :rep/after-evaluation (fn [_w _evaluation] (throw (ex-info "broken hook" {})))}
            (fn [w]
              (let [session (uuid)
                    response (evaluate w session "(def a 20)")]
                (is (= 200 (:status response)))
                (is (= "#'user/a\n" (:body response)))
                (is (= {"rep-status" "done" "rep-exit" "0"}
                       (select-keys (outcome response) ["rep-status" "rep-exit"])))
                (is (= "40\n" (:body (evaluate w session "(* 2 a)")))))))))))
  (testing "the host evaluates in the image after every evaluation and sees what it changed"
    (let [seen (atom [])]
      (with-image {:rep/after-evaluation (fn [w _evaluation]
                                           (swap! seen conj (:value (image/evaluate w "@counter"))))}
        (fn [w]
          (evaluate w (uuid) "(def counter (atom 1))")
          (is (wait-for 2000 #(= 1 (count @seen))))
          (evaluate w (uuid) "(swap! counter inc)")
          (is (wait-for 2000 #(= 2 (count @seen))))
          (is (= ["1" "2"] @seen)))))))

(deftest class-path-test
  (let [extra (str (fs/create-temp-dir {:prefix "rep-extra"}))
        code "(require '[extra.tool :as tool]) (tool/twice 21)"]
    (try
      (fs/create-dirs (fs/path extra "extra"))
      (spit (fs/file extra "extra" "tool.clj") "(ns extra.tool)\n(defn twice [n] (* 2 n))\n")
      (testing "a session's own process finds a namespace on the extra classpath"
        (with-system {:rep/class-path [extra]}
          (fn [w]
            (is (= "42\n" (:body (evaluate w (uuid) code))))
            (testing "the extra entries come after the server's own"
              (is (= (str (pr-str extra) "\n")
                     (:body (evaluate w (uuid) "(last (clojure.string/split (babashka.classpath/get-classpath) #\":\"))"))))))))
      (testing "the shared image finds it too, also by a path relative to the server's folder"
        (with-image {:rep/class-path [(str (fs/relativize (fs/cwd) extra))]}
          (fn [w]
            (is (= "42\n" (:body (evaluate w (uuid) code))))
            (is (= "42" (:value (image/evaluate w code)))))))
      (testing "without the option the namespace is not there"
        (with-system {}
          (fn [w]
            (is (= "failed" (get (:headers (evaluate w (uuid) code)) "rep-status"))))))
      (finally
        (fs/delete-tree extra)))))

(deftest stop-test
  (testing "stop! ends the supervision and the image, and no babashka is left"
    (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
          w (server/start! (server/prepare {:rep/dir folder
                                            :rep/port 0
                                            :rep/token server-test/token
                                            :rep/shared-image? true}))]
      (try
        (let [first-pid (:image/pid (image/status w))
              _ (evaluate w (uuid) "(System/exit 0)")
              second-pid (:image/pid (running w 2))]
          (is (= [second-pid] (nrepl-children)))
          (server/stop! w)
          (is (not (alive? first-pid)))
          (is (not (alive? second-pid)))
          (is (empty? (nrepl-children)))
          (is (= {:image/state :stopped :image/generation 2} (image/status w)))
          (is (= "The image is stopped." (:error (image/evaluate w "(+ 1 2)"))))
          (Thread/sleep 1500)
          (is (empty? (nrepl-children)) "and nothing starts it again"))
        (finally
          (server/stop! w)
          (fs/delete-tree folder)))))
  (testing "also when it comes while the image starts"
    (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
          w (server/prepare {:rep/dir folder :rep/token server-test/token :rep/shared-image? true})
          starting (future (image/start! w))]
      (try
        (is (wait-for 2000 #(:image/stop @(:rep/image w))))
        (image/stop! w)
        (deref starting 5000 nil)
        (is (empty? (nrepl-children)))
        (is (= :stopped (:image/state (image/status w))))
        (finally
          (image/stop! w)
          (fs/delete-tree folder)))))
  (testing "and when the prelude never comes back, so the start hangs"
    (let [prelude (prelude-file "@(promise)\n")
          folder (str (fs/create-temp-dir {:prefix "rep-test"}))
          w (server/prepare {:rep/dir folder
                             :rep/token server-test/token
                             :rep/shared-image? true
                             :rep/prelude prelude})
          starting (future (image/start! w))]
      (try
        (is (wait-for 2000 #(:image/starting @(:rep/image w))))
        (Thread/sleep 700)
        (is (= :starting (:image/state (image/status w))))
        (is (= 1 (count (nrepl-children))))
        (let [begun (System/currentTimeMillis)]
          (image/stop! w)
          (is (< (- (System/currentTimeMillis) begun) 5000)))
        (is (some? (deref starting 5000 nil)) "start! comes back too")
        (is (= :stopped (:image/state (image/status w))))
        (is (empty? (nrepl-children)))
        (finally
          (image/stop! w)
          (fs/delete-tree folder)
          (fs/delete prelude)))))
  (testing "and while the supervisor pauses after a prelude that throws"
    (let [prelude (prelude-file "(throw (ex-info \"broken prelude\" {}))\n")]
      (try
        (with-image {:rep/prelude prelude :rep/backoff-ms 60000 :rep/backoff-max-ms 60000}
          (fn [w]
            (is (= :failed (:image/state (image/status w))))
            (let [begun (System/currentTimeMillis)]
              (server/stop! w)
              (is (< (- (System/currentTimeMillis) begun) 2000) "stop! does not wait for the pause to end"))
            (is (= :stopped (:image/state (image/status w))))
            (is (empty? (nrepl-children)))))
        (finally
          (fs/delete prelude))))))

(deftest rep-path-test
  (testing "the paths that belong to rep"
    (is (every? server/rep-path? ["/rep/a/b" "/rep/" "/file/in/clip.mp4" "/login" "/logout"])))
  (testing "everything else is the host's, the page at / too"
    (is (not-any? server/rep-path? ["/" "" nil "/rep" "/file" "/login/callback" "/logouts"
                                    "/chat/1" "/repository" "/files/a" "/api/rep/a/b"]))))

(deftest host-test
  (testing "a host with an HTTP server of its own mounts the handler and starts the image itself"
    (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
          system (image/start! (server/prepare {:rep/dir folder
                                                :rep/token server-test/token
                                                :rep/shared-image? true}))
          host (http-server/run-server (fn [request]
                                         (if (server/rep-path? (:uri request))
                                           (server/handle system request)
                                           {:status 200 :body (str "the host's page at " (:uri request))}))
                                       {:port 0 :ip "127.0.0.1" :legacy-return-value? false})
          w (assoc system :rep/port (http-server/server-port host))]
      (try
        (is (= :running (:image/state (image/status w))))
        (testing "the host answers its own paths"
          (is (= "the host's page at /" (:body (http/get (server-test/address w "/")))))
          (is (= "the host's page at /chat/1" (:body (http/get (server-test/address w "/chat/1"))))))
        (testing "rep answers the rest: evaluations, the login, files"
          (is (= "3\n" (:body (evaluate w (uuid) "(+ 1 2)"))))
          (is (= 405 (:status (http/get (server-test/address w "/login") {:throw false}))))
          (let [upload (edn/read-string (:body (evaluate w (uuid) "(upload-url \"in/a.txt\")")))]
            (is (str/starts-with? upload (server-test/address w "/file/in/a.txt")))
            (is (= 201 (:status (http/put upload {:body "text" :throw false}))))
            (is (= "\"text\"" (:value (image/evaluate w "(slurp \"in/a.txt\")"))))))
        (finally
          (http-server/server-stop! host)
          (server/stop! w)
          (fs/delete-tree folder)))
      (is (empty? (nrepl-children))))))

(deftest cli-options-test
  (is (= {:rep/shared-image? true :rep/port 9000 :rep/dir "work"}
         (server/cli-options ["--shared-image" "--port" "9000" "--dir" "work"])))
  (is (= {:rep/port 9000} (server/cli-options ["--port" "9000"]))))
