(ns rep.session
  "The babashka process of a session. Every session gets a process of its
   own with an nREPL server in it. The server talks to it over nREPL, a
   person can attach an editor to the same port. The process keeps the
   session's definitions, so they are gone when it ends.

   With `:rep/shared-image?` the sessions get no processes of their own.
   `rep.image` then starts one for all of them, with `spawn!` and
   `prepare!` from here."
  (:require [babashka.classpath :as classpath]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [rep.nrepl :as nrepl]))

(def hidden-variables
  "What a session does not inherit from the server's environment: the
   static token and the key that signs tokens."
  #{"REP_TOKEN" "REP_SECRET"})

(defn environment
  "`env` without the hidden variables."
  [env]
  (apply dissoc (into {} env) hidden-variables))

(defonce ^{:doc "The session processes this server started and has not ended."}
  children
  (atom #{}))

(defonce ^{:private true
           :doc "Ends every session process when the server's own process ends."}
  hook
  (delay
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (run! process/destroy-tree @children))))))

(defn- babashka
  "The babashka that runs this server, so the sessions run the same one."
  []
  (or (some-> (java.lang.ProcessHandle/current)
              (.info)
              (.command)
              (.orElse nil))
      "bb"))

(defn- class-path
  "The server's classpath and after it the entries of `:rep/class-path`,
   all of them absolute. The session starts in the work folder, a
   relative entry would point somewhere else there."
  [w]
  (->> (concat (str/split (or (classpath/get-classpath) "") (re-pattern fs/path-separator))
               (map str (:rep/class-path w)))
       (remove str/blank?)
       (map (comp str fs/absolutize))
       (str/join fs/path-separator)))

(defn- setup-code
  "What a fresh process evaluates before the first request."
  [{:rep/keys [file-secret url-seconds prelude shared-image?]}]
  (str "(require 'rep.prelude)"
       (pr-str (list 'rep.prelude/configure!
                     (cond-> {:secret file-secret
                              :url-seconds url-seconds
                              :parent (.pid (java.lang.ProcessHandle/current))}
                       shared-image? (assoc :shared-image? true))))
       "(require '[rep.prelude :refer [sh upload-url download-url help]])"
       (when prelude
         (pr-str (list 'load-file (str (fs/absolutize prelude)))))))

(defn- forward!
  "Copies what the process writes to its own standard output to the
   server's standard error, so that it does not vanish. Output that the
   code prints goes into the answers instead and never comes by here."
  [lines]
  (try
    (loop []
      (when-let [line (.readLine lines)]
        (when-not (str/starts-with? line "For more info visit")
          (.println System/err line))
        (recur)))
    (catch java.io.IOException _ nil)))

(defn end!
  "Ends the process and waits a moment until it is gone. On its way out
   babashka removes the `.nrepl-port` file it keeps in the work folder,
   and a caller that deletes that folder next should not race with it."
  [child]
  (process/destroy-tree child)
  (.waitFor (:proc child) 3 java.util.concurrent.TimeUnit/SECONDS)
  (swap! children disj child)
  nil)

(defn spawn!
  "Starts a babashka with an nREPL server in the work folder and answers
   its process. `prepare!` makes it ready for evaluations, `end!` ends
   it."
  [{:rep/keys [dir] :as w}]
  (force hook)
  (let [child (process/process {:cmd [(babashka) "-cp" (class-path w) "nrepl-server" "127.0.0.1:0"]
                                :dir dir
                                :err :out
                                :env (environment (System/getenv))})]
    (swap! children conj child)
    (.close (:in child))
    child))

(defn- ready!
  "Reads the port from what the fresh process `child` prints first and
   runs the setup code in it. Answers the process with its nREPL port,
   or `:session/error` with the reason."
  [w session child begun]
  (let [lines (java.io.BufferedReader. (java.io.InputStreamReader. (:out child) "UTF-8"))
        banner (deref (future (.readLine lines)) 15000 nil)
        port (some->> banner (re-find #":(\d+)") (second) (parse-long))]
    (if-not port
      (do
        (end! child)
        {:session/error (str "The session's babashka did not start: " (pr-str banner))})
      (let [_ (future (forward! lines))
            connection (nrepl/connect port)
            answers (try
                      (nrepl/call! connection {"op" "eval"
                                               "session" (nrepl/clone! connection)
                                               "code" (setup-code w)})
                      (finally
                        (nrepl/close! connection)))
            problem (some :err answers)]
        (if problem
          (do
            (end! child)
            {:session/error (str "The session's prelude failed: " (str/trim problem))})
          {:session/id session
           :session/process child
           :session/port port
           :session/base-url (atom nil)
           :session/start-ms (- (System/currentTimeMillis) begun)})))))

(defn prepare!
  "Makes the process `child` of `spawn!` ready for the evaluations of
   `session`: waits for its nREPL server and runs the setup code, the
   prelude included. `begun` is the millisecond its start began. Answers
   the process with its nREPL port, or `:session/error` with the reason.
   A process that breaks on the way is ended before the exception goes
   on."
  [w session child begun]
  (try
    (ready! w session child begun)
    (catch Throwable e
      (end! child)
      (throw e))))

(defn- start!
  "Starts a process for `session` and prepares it. Answers the process
   with its nREPL port, or `:session/error` with the reason."
  [w session]
  (let [begun (System/currentTimeMillis)]
    (prepare! w session (spawn! w) begun)))

(defn alive?
  [{:session/keys [process]}]
  (boolean (some-> process :proc (.isAlive))))

(defn- usable?
  "True for a process that runs, and for one that is starting right now."
  [entry]
  (or (not (realized? entry))
      (alive? @entry)))

(defn state
  "What the session is to this server: :continued when its process runs,
   :restarted when only its folder on disk is left (`known?`), :started
   when it is new."
  [{:rep/keys [sessions]} session known?]
  (cond
    (some-> (get @sessions session) usable?) :continued
    known? :restarted
    :else :started))

(defn reserve!
  "Makes sure `session` has a process or is about to get one. The process
   itself starts when somebody asks for it with `process!`."
  [{:rep/keys [sessions] :as w} session]
  (swap! sessions
         (fn [all]
           (let [entry (get all session)]
             (if (some-> entry usable?)
               all
               (do
                 (when (some-> entry realized?)
                   (some->> (:session/process @entry) (swap! children disj)))
                 (assoc all session (delay (start! w session))))))))
  nil)

(defn process!
  "The running process of `session`, started when it has none. Check the
   answer for `:session/error`."
  [{:rep/keys [sessions] :as w} session]
  (reserve! w session)
  @(get @sessions session))

(defn base-url!
  "Tells the session where the caller reached the server, when that
   changed. The URLs it hands out start there."
  [{:session/keys [base-url]} connection nrepl-session url]
  (when (not= url @base-url)
    (nrepl/call! connection {"op" "eval"
                             "session" nrepl-session
                             "code" (pr-str (list 'rep.prelude/base-url! url))})
    (reset! base-url url))
  nil)

(defn stop-all!
  "Ends every session process of this system."
  [{:rep/keys [sessions]}]
  (doseq [entry (vals @sessions)
          :when (realized? entry)
          :let [{:session/keys [process]} @entry]
          :when process]
    (end! process))
  (reset! sessions {})
  nil)

(comment
  (def w {:rep/dir "work" :rep/file-secret "secret" :rep/url-seconds 900 :rep/sessions (atom {})})
  (def session (process! w "s1"))
  (:session/port session)
  (:session/start-ms session)
  (state w "s1" true)
  (stop-all! w)

  ;; a process that is not kept under a session uuid, with more on its classpath
  (def child (spawn! (assoc w :rep/class-path ["examples"])))
  (def started (prepare! w "one-off" child (System/currentTimeMillis)))
  (alive? started)
  (end! child))
