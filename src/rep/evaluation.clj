(ns rep.evaluation
  "One evaluation of a session. It has a folder on disk with the request,
   the output and a status, and while it runs a thread of its own that
   talks to the session's babashka over nREPL. The id in the URL names
   it. A second request with the same id never runs the code again, it
   gets the answer of the first. `sweep!` deletes the folders of the
   evaluations that are older than `:rep/keep-days`.

   The babashka is the session's own process, or with
   `:rep/shared-image?` the one image that all sessions share."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [rep.hook :as hook]
            [rep.image :as image]
            [rep.nrepl :as nrepl]
            [rep.output :as output]
            [rep.session :as session]))

(defn session-folder
  [{:rep/keys [state-dir]} session]
  (fs/path state-dir "sessions" session))

(defn folder
  "Where the evaluation `id` of `session` keeps its files."
  [w session id]
  (fs/path (session-folder w session) id))

(defn output-file
  [w session id]
  (fs/file (folder w session id) "output"))

(defn sha256
  "The SHA-256 of `text` in hex."
  [text]
  (->> (.digest (java.security.MessageDigest/getInstance "SHA-256")
                (.getBytes (str text) "UTF-8"))
       (map #(format "%02x" %))
       (apply str)))

(defn- write-record!
  "Replaces the status file in one move, so a reader never sees half of it."
  [w session id record]
  (let [target (fs/path (folder w session id) "status.edn")
        fresh (fs/path (folder w session id) "status.edn.new")]
    (spit (fs/file fresh) (pr-str record))
    (fs/move fresh target {:replace-existing true :atomic-move true})))

(defn- read-record
  [w session id]
  (let [file (fs/file (folder w session id) "status.edn")]
    (when (fs/exists? file)
      (edn/read-string (slurp file)))))

(defn running
  "The evaluation `id` of `session` while it runs, else nil."
  [{:rep/keys [evaluations]} session id]
  (get @evaluations [session id]))

(defn status
  "What is known about the evaluation: `:state` is :running, :done,
   :failed or :interrupted, `:exit` is 0 or 1. An evaluation that the
   record calls running while no thread runs it was cut off by a restart
   of the server, and nobody knows how far it got. Nil for an id that
   was never used."
  [w session id]
  (if (running w session id)
    {:state :running}
    (when-let [record (read-record w session id)]
      (if (= :running (:state record))
        (assoc record :state :interrupted)
        record))))

(defn- follow!
  "Reads the answers to the eval `id` until it is done. What the code
   prints goes into the output as it comes, the error text of babashka
   too. Answers `:value`, the printed value of the last form, and
   `:failed?`."
  [connection id output]
  (loop [result {}]
    (let [{:keys [out err value status] :as message} (nrepl/receive! connection)]
      (if (not= id (:id message))
        (recur result)
        (let [_ (some->> out (output/print! output))
              _ (some->> err (output/print! output))
              result (cond-> result
                       value (assoc :value value)
                       (some #{"eval-error" "interrupted"} status) (assoc :failed? true))]
          (if (nrepl/done? message)
            result
            (recur result)))))))

(defn- drain!
  "Reads on until the eval `id` is done and drops what comes. Gives up
   after a few seconds without a message."
  [{:nrepl/keys [socket] :as connection} id]
  (try
    (.setSoTimeout socket 5000)
    (loop []
      (let [message (nrepl/receive! connection)]
        (when-not (and (= id (:id message)) (nrepl/done? message))
          (recur))))
    (catch java.io.IOException _ nil)))

(defn- error-data
  "The data of the exception that just ended the code, printed, or nil."
  [connection nrepl-session]
  (let [value (some :value (nrepl/call! connection {"op" "eval"
                                                    "session" nrepl-session
                                                    "code" "(rep.prelude/error-data *e)"}))]
    (when (and value (not= "nil" value))
      value)))

(defn- session-state
  "Whether `session` is :started, :continued or :restarted, in its own
   process or in the shared image."
  [{:rep/keys [shared-image?] :as w} session]
  (let [known? (fs/exists? (session-folder w session))]
    (if shared-image?
      (image/state w session known?)
      (session/state w session known?))))

(defn- reserve!
  "Makes sure that the evaluation of `session` that comes next finds the
   session :continued."
  [{:rep/keys [shared-image?] :as w} session]
  (if shared-image?
    (image/reserve! w session)
    (session/reserve! w session)))

(defn- process!
  "The process that evaluates for `session`: its own, or the shared
   image. Check the answer for `:session/error`."
  [{:rep/keys [shared-image?] :as w} session]
  (if shared-image?
    (image/process! w session)
    (session/process! w session)))

(defn- evaluate!
  "Runs `body` in the session's process and answers the outcome. Every
   evaluation has a connection and an nREPL session of its own, so that
   two of them run at the same time."
  [w {:evaluation/keys [session id output]} body]
  (let [{:session/keys [port error]} (process! w session)]
    (if error
      (do
        (output/print! output (str error "\n"))
        {:state :failed :exit 1})
      (let [connection (nrepl/connect port)]
        (try
          (let [nrepl-session (nrepl/clone! connection)
                _ (nrepl/send! connection {"op" "eval"
                                           "id" id
                                           "session" nrepl-session
                                           "code" (nrepl/wrapped body)})
                {:keys [value failed?]} (try
                                          (follow! connection id output)
                                          (catch clojure.lang.ExceptionInfo e
                                            (nrepl/send! connection {"op" "interrupt"
                                                                     "id" "interrupt"
                                                                     "session" nrepl-session
                                                                     "interrupt-id" id})
                                            (drain! connection id)
                                            (try
                                              (nrepl/call! connection {"op" "close" "session" nrepl-session})
                                              (catch java.io.IOException _ nil))
                                            (throw e)))
                outcome (if failed?
                          (do
                            (some->> (error-data connection nrepl-session)
                                     (output/print-line! output))
                            {:state :failed :exit 1})
                          (do
                            (when (and value (not= "nil" value))
                              (output/print-line! output value))
                            {:state :done :exit 0}))]
            (nrepl/call! connection {"op" "close" "session" nrepl-session})
            outcome)
          (finally
            (nrepl/close! connection)))))))

(defn- problem
  "What the answer says when the evaluation broke outside of the code."
  [e]
  (cond
    (= :output-limit (:rep/error (ex-data e))) nil
    (instance? java.io.IOException e) "The session's process ended. Its definitions are gone."
    :else (str "rep: " (or (ex-message e) (.getName (class e))))))

(defn- finish!
  "Records the outcome and lets everyone who follows go."
  [{:rep/keys [evaluations] :as w}
   {:evaluation/keys [session id output done record]}
   outcome]
  (write-record! w session id (merge record outcome {:ended (System/currentTimeMillis)}))
  (swap! evaluations dissoc [session id])
  (output/close! output)
  (deliver done outcome))

(defn- run
  "The whole life of an evaluation on its thread. At its end the hook
   `:rep/after-evaluation` hears about it, once the outcome is on record
   and everyone who follows has the answer."
  [w {:evaluation/keys [session id output] :as evaluation} body]
  (let [outcome (try
                  (evaluate! w evaluation body)
                  (catch Throwable e
                    (try
                      (some->> (problem e) (output/print-line! output))
                      (catch Throwable _ nil))
                    {:state :failed :exit 1}))]
    (try
      (finish! w evaluation outcome)
      (catch Throwable e
        (binding [*out* *err*]
          (println "rep: could not record the outcome of" (:evaluation/id evaluation) (ex-message e)))))
    (hook/call! w :rep/after-evaluation {:evaluation/session session
                                         :evaluation/id id
                                         :evaluation/state (:state outcome)
                                         :evaluation/exit (:exit outcome)
                                         :evaluation/code body})))

(defn- start!
  "Makes the evaluation's folder and record, and starts its thread."
  [{:rep/keys [evaluations output-limit] :as w}
   {:request/keys [session id body]}]
  (let [state (session-state w session)
        dir (folder w session id)
        record {:state :running
                :sha256 (sha256 body)
                :started (System/currentTimeMillis)}
        _ (fs/create-dirs dir)
        _ (spit (fs/file dir "request") body)
        _ (write-record! w session id record)
        _ (reserve! w session)
        evaluation {:evaluation/session session
                    :evaluation/id id
                    :evaluation/record record
                    :evaluation/output (output/open (output-file w session id) output-limit)
                    :evaluation/done (promise)}]
    (swap! evaluations assoc [session id] evaluation)
    (future (run w evaluation body))
    {:submit/answer :started
     :submit/session state
     :submit/evaluation evaluation}))

(defn submit!
  "Takes a request to evaluate the code `:request/body` as evaluation
   `:request/id` of `:request/session`, and adds `:submit/answer`:

   :started   a new evaluation runs now. `:submit/session` tells whether
              the session is :started, :continued or :restarted, and
              `:submit/evaluation` is the evaluation.
   :replayed  the id was used before with this very request. Nothing runs
              again, the caller answers from what is there.
   :conflict  the id was used before with another request.

   A session takes a new request while an earlier one still runs."
  [{:rep/keys [lock] :as w} {:request/keys [session id body] :as request}]
  (locking lock
    (let [known (or (some-> (running w session id) :evaluation/record)
                    (read-record w session id))]
      (merge w
             request
             (cond
               (and known (not= (sha256 body) (:sha256 known)))
               {:submit/answer :conflict}

               known
               {:submit/answer :replayed}

               :else
               (start! w request))))))

(def keep-days
  "How many days an evaluation stays on disk when the system names no
   `:rep/keep-days`."
  7)

(defn- last-mattered
  "The millisecond the evaluation in the folder `dir` ended, else the
   one it started, else the one its folder last changed."
  [dir]
  (let [record (try
                 (edn/read-string (slurp (fs/file dir "status.edn")))
                 (catch Exception _ nil))]
    (or (:ended record)
        (:started record)
        (fs/file-time->millis (fs/last-modified-time dir)))))

(defn- drop-empty!
  "Removes the folder of a session that holds no evaluation any more.
   Under the lock of `submit!`, so that a new evaluation never loses the
   folder it is about to write into."
  [{:rep/keys [lock]} dir]
  (locking lock
    (when (empty? (fs/list-dir dir))
      (fs/delete dir))))

(defn sweep!
  "Deletes the evaluations that ended more than `:rep/keep-days` ago:
   the folder with the request, the output and the outcome. An id that
   old is forgotten, the same POST would run its code again. The clock
   is `:rep/now-ms` in epoch milliseconds, the system's clock without
   it. An evaluation that runs right now stays, and so does everything
   when `:rep/keep-days` is 0. A session whose last evaluation went
   loses its folder too, so it counts as new when it shows up again.
   Answers how many evaluations it deleted."
  [{:rep/keys [state-dir now-ms] :as w}]
  (let [days (or (:rep/keep-days w) keep-days)
        before (- (or now-ms (System/currentTimeMillis)) (* days 24 60 60 1000))
        sessions (fs/path state-dir "sessions")]
    (if (and (pos? days) (fs/directory? sessions))
      (reduce
       (fn [deleted session-dir]
         (let [session (fs/file-name session-dir)
               old (->> (fs/list-dir session-dir)
                        (filter fs/directory?)
                        (remove #(running w session (fs/file-name %)))
                        (filter #(< (last-mattered %) before)))]
           (run! fs/delete-tree old)
           (drop-empty! w session-dir)
           (+ deleted (count old))))
       0
       (filter fs/directory? (fs/list-dir sessions)))
      0)))

(comment
  (require '[rep.server :as server])
  (def w (server/prepare {:rep/dir "work"
                          :rep/after-evaluation (fn [_w info] (prn info))}))
  (sweep! w)
  (sweep! (assoc w :rep/now-ms (+ (System/currentTimeMillis) (* 8 24 60 60 1000))))
  (def answer (submit! w {:request/session "s1" :request/id "e1"
                          :request/body "(+ 1 2)"}))
  @(:evaluation/done (:submit/evaluation answer))
  (status w "s1" "e1")
  (slurp (output-file w "s1" "e1"))
  (session/stop-all! w))
