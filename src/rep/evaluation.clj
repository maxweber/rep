(ns rep.evaluation
  "One evaluation of a session. It has a folder on disk with the request,
   the output and a status, and while it runs a thread of its own. The
   id in the URL names it. A second request with the same id never runs
   the code again, it gets the answer of the first."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [rep.context :as context]
            [rep.output :as output]
            [sci.core :as sci]))

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
   :failed, :stopped or :interrupted, `:exit` its exit code. An evaluation
   that the record calls running while no thread runs it was cut off by a
   restart of the server, and nobody knows how far it got. Nil for an id
   that was never used."
  [w session id]
  (if (running w session id)
    {:state :running}
    (when-let [record (read-record w session id)]
      (if (= :running (:state record))
        (assoc record :state :interrupted)
        record))))

(defn- busy
  "The id of the evaluation that runs in `session`, if one does."
  [{:rep/keys [evaluations]} session]
  (some (fn [[[running-session id]]]
          (when (= session running-session)
            id))
        @evaluations))

(defn- session-state
  "What the session is to this server: :continued when its context is in
   memory, :restarted when only its folder is left, :started when it is new."
  [{:rep/keys [sessions] :as w} session]
  (cond
    (contains? @sessions session) :continued
    (fs/exists? (session-folder w session)) :restarted
    :else :started))

(defn- context!
  "The SCI context of `session`, made on first use."
  [{:rep/keys [sessions] :as w} session]
  (or (get @sessions session)
      (get (swap! sessions assoc session (context/create w)) session)))

(defn error-text
  "What the answer says about an exception, as a REPL would."
  [e]
  (let [{:keys [type line column] :as data} (ex-data e)
        sci? (contains? #{:sci/error :sci.error/parse} type)]
    (str "Error: " (or (ex-message e) (.getName (class e)))
         (when (and sci? line)
           (str " (line " line ", column " column ")"))
         (when-let [shown (and (not sci?) (not-empty (dissoc data :rep/error)))]
           (str "\n" (pr-str shown))))))

(defn- stream
  "An OutputStream that writes into the evaluation's output and refuses
   once the evaluation was told to stop."
  [{:evaluation/keys [output stopped?]}]
  (let [write! (fn [data offset length]
                 (when @stopped?
                   (throw (ex-info "Stopped." {:rep/error :stopped})))
                 (output/write! output data offset length))]
    (proxy [java.io.OutputStream] []
      (write
        ([data]
         (if (bytes? data)
           (write! data 0 (alength data))
           (write! (byte-array [(unchecked-byte data)]) 0 1)))
        ([data offset length]
         (write! data offset length))))))

(defn- finish!
  "Records the outcome and lets everyone who follows go. The first call
   wins, a later one changes nothing."
  [{:rep/keys [evaluations] :as w}
   {:evaluation/keys [session id output finished? done record]}
   outcome]
  (when (compare-and-set! finished? false true)
    (write-record! w session id (merge record outcome {:ended (System/currentTimeMillis)}))
    (swap! evaluations dissoc [session id])
    (output/close! output)
    (deliver done outcome)))

(defn- run
  "The whole life of an evaluation on its thread. The printed text and
   the value go into the output. As with `bb -e`, nil is not printed. A
   bash result is not printed either, its text is there already, and its
   exit code becomes the evaluation's."
  [w ctx {:evaluation/keys [stopped?] :as evaluation} body]
  (let [writer (java.io.OutputStreamWriter. (stream evaluation) "UTF-8")
        outcome (try
                  (binding [context/*evaluation* evaluation
                            *out* writer
                            *err* writer]
                    (let [value (sci/eval-string* ctx body)]
                      (when-not (or (nil? value) (context/quiet? value))
                        (prn value))
                      (flush)
                      {:state :done
                       :exit (if (context/quiet? value)
                               (:exit value 0)
                               0)}))
                  (catch Throwable e
                    (Thread/interrupted)
                    (if @stopped?
                      {:state :stopped :exit 130}
                      (do
                        (try
                          (binding [*out* writer]
                            (println (error-text e))
                            (flush))
                          (catch Throwable _ nil))
                        {:state :failed :exit 1}))))
        outcome (if @stopped?
                  {:state :stopped :exit 130}
                  outcome)]
    (try
      (finish! w evaluation outcome)
      (catch Throwable e
        (binding [*out* *err*]
          (println "rep: could not record the outcome of" (:evaluation/id evaluation) (ex-message e)))))))

(defn- start!
  "Makes the evaluation's folder and record, and starts its thread."
  [{:rep/keys [evaluations output-limit] :as w}
   {:request/keys [session id body base-url]}]
  (let [state (session-state w session)
        dir (folder w session id)
        record {:state :running
                :sha256 (sha256 body)
                :started (System/currentTimeMillis)}
        _ (fs/create-dirs dir)
        _ (spit (fs/file dir "request") body)
        _ (write-record! w session id record)
        evaluation {:evaluation/session session
                    :evaluation/id id
                    :evaluation/base-url base-url
                    :evaluation/record record
                    :evaluation/output (output/open (output-file w session id) output-limit)
                    :evaluation/process (atom nil)
                    :evaluation/thread (atom nil)
                    :evaluation/stopped? (atom false)
                    :evaluation/finished? (atom false)
                    :evaluation/done (promise)}
        ctx (context! w session)]
    (swap! evaluations assoc [session id] evaluation)
    (reset! (:evaluation/thread evaluation)
            (future (run w ctx evaluation body)))
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
   :busy      the session runs `:submit/running`. A session evaluates
              one request at a time."
  [{:rep/keys [lock] :as w} {:request/keys [session id body] :as request}]
  (locking lock
    (let [known (or (some-> (running w session id) :evaluation/record)
                    (read-record w session id))
          running-id (busy w session)]
      (merge w
             request
             (cond
               (and known (not= (sha256 body) (:sha256 known)))
               {:submit/answer :conflict}

               known
               {:submit/answer :replayed}

               running-id
               {:submit/answer :busy :submit/running running-id}

               :else
               (start! w request))))))

(defn stop!
  "Ends the evaluation `id` of `session` when it runs, and adds
   `:stop/answer` :stopped, :finished (it was over before) or :unknown.
   The bash process is killed and the thread interrupted. Code that
   computes without printing or calling anything does not notice an
   interrupt. Such an evaluation is recorded as stopped all the same, and
   its thread is left to itself."
  [w session id]
  (if-let [{:evaluation/keys [stopped? process thread done] :as evaluation} (running w session id)]
    (do
      (reset! stopped? true)
      (some-> @process process/destroy-tree)
      (some-> @thread future-cancel)
      (when (= ::waiting (deref done 2000 ::waiting))
        (finish! w evaluation {:state :stopped :exit 130}))
      (assoc w :stop/answer :stopped))
    (assoc w :stop/answer (if (read-record w session id) :finished :unknown))))

(comment
  (require '[rep.server :as server])
  (def w (server/prepare {:rep/dir "work"}))
  (def answer (submit! w {:request/session "s1" :request/id "e1"
                          :request/body "(+ 1 2)"}))
  @(:evaluation/done (:submit/evaluation answer))
  (status w "s1" "e1")
  (slurp (output-file w "s1" "e1")))
