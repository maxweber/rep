(ns rep.client
  "The command line client. It reads code from standard input, so the
   code needs no quoting on the way, prints the answer as it comes and
   ends with 0 when the code ran through and 1 when it failed.

     rep <<'EOF'
     (+ 1 2)
     EOF

   It makes the evaluation id itself and sends the same id when it has to
   ask again, after a broken connection or for the rest of a long answer.
   The server runs the code once."
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

(defn- post
  "POSTs `code` as the evaluation `id` and reads the answer as a stream,
   from byte `from` on."
  [{:client/keys [url token session]} id code from]
  (http/post (str url "/rep/" session "/" id (when (pos? from) (str "?from=" from)))
             {:headers {"Authorization" (str "Bearer " token)
                        "Content-Type" "application/clojure"}
              :body code
              :as :stream
              :throw false}))

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
  "One POST. A connection that fails before an answer came is tried
   again a few times. The id is the same, so nothing runs twice."
  [w id code from]
  (loop [tries 0]
    (let [response (try
                     (post w id code from)
                     (catch java.io.IOException e e))]
      (if (instance? Throwable response)
        (if (< tries 3)
          (do
            (Thread/sleep (* 500 (inc tries)))
            (recur (inc tries)))
          (throw response))
        response))))

(defn follow
  "Evaluates `code` as `id` and follows its output to the end. An answer
   that ends while the evaluation still runs is asked for again, from
   where it stopped. Adds `:client/status` and `:client/exit`."
  [w id code]
  (loop [response (attempt w id code 0)
         received 0
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
          (when (and (zero? rounds) (= "replayed" (get headers "rep-evaluation")))
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
            (recur (attempt w id code received)
                   received
                   (inc rounds))))))))

(defn- finish
  [{:client/keys [status exit]}]
  (when (= "interrupted" status)
    (note "rep: the server restarted while this ran. Nobody knows how far it got."))
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
          (note (str "rep: still running after " seconds " s. "
                     "Send the same input again with --id " id " to follow it."))
          (System/exit 124))
        result))))

(def cli-spec
  {:id {:desc "Evaluation id, a fresh UUID by default"}
   :session-id {:desc "Session id (REP_SESSION, else the file .rep-session)"}
   :url {:desc "Server address (REP_URL, else http://127.0.0.1:8730)"}
   :token {:desc "Bearer token (REP_TOKEN)"}
   :wait {:coerce :long :desc "Stop following after this many seconds, exit 124"}})

(def help
  (str/join
   "\n"
   ["rep [--id ID] [--wait SECONDS] < code   evaluates standard input, prints the answer"
    "rep session                             prints the session id"
    ""
    (cli/format-opts {:spec cli-spec})]))

(defn -main
  [& args]
  (let [{:keys [opts args]} (cli/parse-args args {:spec cli-spec})
        [command] args]
    (if (:help opts)
      (println help)
      (let [w (settings opts)]
        (when-not (:client/token w)
          (note "rep: no token. Set REP_TOKEN or pass --token.")
          (System/exit 2))
        (case command
          "session" (println (:client/session w))
          nil (let [id (or (:id opts) (str (random-uuid)))
                    code (slurp *in*)]
                (finish (within (:wait opts) id #(follow w id code))))
          (do
            (note help)
            (System/exit 2)))))))
