(ns rep.client
  "The command line client. It reads code from standard input, so the
   code needs no quoting, prints the answer as it comes and ends with the
   evaluation's exit code.

     rep <<'EOF'
     (+ 1 2)
     EOF

     rep --sh <<'EOF'
     ffmpeg -version | head -1
     EOF

   It makes the evaluation id itself and uses the same id when it has to
   ask again after a broken connection, so a retry never runs the code a
   second time."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.string :as str]))

(def session-file
  ".rep-session")

(defn- note
  "A line for the person or agent at the terminal, on standard error."
  [& words]
  (binding [*out* *err*]
    (apply println words)))

(defn session
  "The session id: the option, `REP_SESSION`, or the one kept in the file
   `.rep-session` of the current folder, which is made on first use."
  [given]
  (or given
      (not-empty (System/getenv "REP_SESSION"))
      (when (fs/exists? session-file)
        (not-empty (str/trim (slurp session-file))))
      (let [fresh (str (random-uuid))]
        (spit session-file (str fresh "\n"))
        fresh)))

(defn- settings
  [{:keys [url token session-id]}]
  {:client/url (str/replace (or url (System/getenv "REP_URL") "http://127.0.0.1:8730") #"/+$" "")
   :client/token (or token (System/getenv "REP_TOKEN"))
   :client/session (session session-id)})

(defn- address
  [{:client/keys [url session]} id]
  (str url "/rep/" session "/" id))

(defn- request
  "One HTTP request whose answer is read as a stream."
  [{:client/keys [token] :as w} id {:keys [method body shell? from]}]
  (http/request
   (cond-> {:method method
            :uri (str (address w id) (when from (str "?from=" from)))
            :headers (cond-> {"Authorization" (str "Bearer " token)}
                       body (assoc "Content-Type" (if shell?
                                                    "text/x-shellscript"
                                                    "application/clojure")))
            :as :stream
            :throw false}
     body (assoc :body body))))

(defn- copy!
  "Copies the answer to standard output as it arrives. Answers how many
   bytes came, and the error when the connection broke on the way."
  [in]
  (let [buffer (byte-array 8192)]
    (loop [total 0]
      (let [n (try
                (.read in buffer)
                (catch java.io.IOException e e))]
        (cond
          (instance? Throwable n) {:bytes total :broken n}
          (neg? n) {:bytes total}
          :else (do
                  (.write System/out buffer 0 n)
                  (.flush System/out)
                  (recur (+ total n))))))))

(defn- attempt
  "Sends `first-request` and reads its answer. A connection that fails
   before an answer came is tried again a few times with the same id."
  [w id first-request]
  (loop [tries 0]
    (let [response (try
                     (request w id first-request)
                     (catch java.io.IOException e e))]
      (if (instance? Throwable response)
        (if (< tries 3)
          (do
            (Thread/sleep (* 500 (inc tries)))
            (recur (inc tries)))
          (throw response))
        response))))

(defn follow
  "Runs `first-request` for the evaluation `id` and follows its output to
   the end. Adds `:client/status` and `:client/exit`."
  [w id first-request]
  (loop [response (attempt w id first-request)
         received (or (:from first-request) 0)
         rounds 0]
    (let [{:keys [status headers body]} response]
      (if (not= 200 status)
        (do
          (note (str/trim (slurp body)))
          (assoc w :client/status "refused" :client/exit 2))
        (let [{:keys [bytes broken]} (copy! body)
              received (+ received bytes)
              state (get headers "rep-status")]
          (when (and (zero? rounds) (= "restarted" (get headers "rep-session")))
            (note "rep: the session started over. Earlier definitions are gone."))
          (when (and (zero? rounds) (= "replayed" (get headers "rep-evaluation")) (:body first-request))
            (note "rep: this id was evaluated before. This is its answer, nothing ran again."))
          (cond
            (and (not broken) (not= "running" state))
            (assoc w
                   :client/status state
                   :client/exit (or (some-> (get headers "rep-exit") parse-long)
                                    (if (= "done" state) 0 1)))

            (> rounds 1000)
            (assoc w :client/status "running" :client/exit 1)

            :else
            (recur (attempt w id {:method :get :from received})
                   received
                   (inc rounds))))))))

(defn- finish
  [{:client/keys [status exit]}]
  (case status
    "interrupted" (note "rep: the server restarted while this ran. Nobody knows how far it got.")
    "stopped" (note "rep: stopped.")
    nil)
  (System/exit exit))

(defn- within
  "Runs `work` and ends the client with 124 when it takes longer than
   `seconds`. The evaluation goes on at the server."
  [seconds id work]
  (if-not seconds
    (work)
    (let [result (deref (future (work)) (* 1000 seconds) ::late)]
      (if (= ::late result)
        (do
          (note (str "rep: still running after " seconds " s. Follow it with: rep attach " id))
          (System/exit 124))
        result))))

(def cli-spec
  {:sh {:coerce :boolean :desc "The input is a bash script"}
   :id {:desc "Evaluation id, a fresh UUID by default"}
   :session-id {:desc "Session id (REP_SESSION, else the file .rep-session)"}
   :url {:desc "Server address (REP_URL, else http://127.0.0.1:8730)"}
   :token {:desc "Bearer token (REP_TOKEN)"}
   :wait {:coerce :long :desc "Stop following after this many seconds, exit 124"}})

(def help
  (str/join
   "\n"
   ["rep [--sh] [--wait SECONDS] < code   evaluates standard input, prints the answer"
    "rep attach ID                        follows an evaluation or prints it again"
    "rep stop ID                          stops an evaluation"
    "rep session                          prints the session id"
    ""
    (cli/format-opts {:spec cli-spec})]))

(defn -main
  [& args]
  (let [{:keys [opts args]} (cli/parse-args args {:spec cli-spec})
        [command id] args]
    (if (:help opts)
      (println help)
      (let [w (settings opts)]
        (when-not (:client/token w)
          (note "rep: no token. Set REP_TOKEN or pass --token.")
          (System/exit 2))
        (case command
          "session" (println (:client/session w))
          "attach" (finish (within (:wait opts) id #(follow w id {:method :get :from 0})))
          "stop" (System/exit (if (= "refused" (:client/status (follow w id {:method :delete}))) 2 0))
          nil (let [id (or (:id opts) (str (random-uuid)))
                    code (slurp *in*)]
                (finish (within (:wait opts) id
                                #(follow w id {:method :post :body code :shell? (:sh opts)}))))
          (do
            (note help)
            (System/exit 2)))))))
