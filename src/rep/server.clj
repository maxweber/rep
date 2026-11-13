(ns rep.server
  "REP over HTTP: read, eval, print, and the loop stays with the caller.

     POST /rep/<session>/<id>   evaluates the body, answers its output
     PUT  /file/<path>?...      a signed upload into the work folder
     GET  /file/<path>?...      a signed download out of it

   The body of the POST is Clojure code for the session, a babashka
   process of its own. The answer is plain text: what the code printed
   and the value of its last form, with the output of bash as bash wrote
   it. The outcome travels in the headers Rep-Status and Rep-Exit, so the
   text needs no frame around it. The same POST again runs nothing, it
   answers the same text, from byte `?from=` on when asked."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [rep.evaluation :as evaluation]
            [rep.files :as files]
            [rep.output :as output]
            [rep.session :as session]))

(def text-headers
  {"Content-Type" "text/plain; charset=utf-8"})

(def usage
  "What a GET on / answers, for an agent that only knows the address."
  (str/join
   "\n"
   ["rep: read, eval, print. You keep the loop."
    ""
    "POST /rep/<session-uuid>/<evaluation-uuid>   body = Clojure code, answer = plain text"
    ""
    "Send the header Authorization: Bearer <token>."
    "Pick one session uuid and keep it. A session is a babashka process, definitions stay in it."
    "Use a new evaluation uuid for every new piece of code."
    "The same uuid with the same code answers the same text again and runs nothing. Add ?from=<bytes> for the rest only."
    "The headers Rep-Status (running, done, failed, interrupted) and Rep-Exit (0 or 1) tell the outcome."
    "Long work: start it in a future, keep the result in an atom, ask in a later request."
    "Start with the code (help)."
    ""]))

(defn- text
  ([status body]
   (text status body nil))
  ([status body headers]
   {:status status
    :headers (merge text-headers headers)
    :body body}))

(defn- authorized?
  "True when the request carries the server's token as a bearer."
  [{:rep/keys [token]} request]
  (let [given (some-> (get-in request [:headers "authorization"])
                      (str/replace-first #"(?i)^bearer\s+" ""))]
    (boolean
     (and given
          (java.security.MessageDigest/isEqual (.getBytes given "UTF-8")
                                               (.getBytes (str token) "UTF-8"))))))

(defn- base-url
  "Where the caller reached the server, for the URLs handed out to it."
  [{:rep/keys [public-url]} {:keys [headers scheme]}]
  (or public-url
      (str (or (get headers "x-forwarded-proto") (name (or scheme :http)))
           "://"
           (get headers "host"))))

(defn- from
  "The byte the caller wants the output from."
  [request]
  (max 0 (or (some-> (get (files/query (:query-string request)) "from") parse-long)
             0)))

(defn- answer
  "The output of an evaluation from byte `from` on. While the evaluation
   runs, the answer stays open and carries every chunk as it comes."
  [w request session id from headers]
  (if-let [running (evaluation/running w session id)]
    (http/as-channel
     request
     {:on-open (fn [channel]
                 (http/send! channel
                             {:status 200
                              :headers (merge text-headers headers {"Rep-Status" "running"})}
                             false)
                 (output/follow! (:evaluation/output running) channel from))
      :on-close (fn [channel _status]
                  (output/leave! (:evaluation/output running) channel))})
    (if-let [{:keys [state exit]} (evaluation/status w session id)]
      {:status 200
       :headers (merge text-headers
                       headers
                       {"Rep-Status" (name state)}
                       (when exit
                         {"Rep-Exit" (str exit)}))
       :body (java.io.ByteArrayInputStream.
              (output/read-from (evaluation/output-file w session id) from))}
      (text 404 "No such evaluation.\n"))))

(defn- evaluate
  "Answers a POST. A new evaluation gets a moment to finish, so that a
   quick one answers with its outcome in the headers right away."
  [{:rep/keys [grace-ms] :as w} request session id]
  (let [body (some-> (:body request) (.readAllBytes) (String. "UTF-8"))]
    (if (str/blank? body)
      (text 400 "The body is empty. Send the code to evaluate.\n")
      (let [submitted (evaluation/submit! w {:request/session session
                                             :request/id id
                                             :request/body body
                                             :request/base-url (base-url w request)})]
        (case (:submit/answer submitted)
          :conflict
          (text 409 (str "The evaluation id " id " was used for another request. "
                         "Use a new id for new code.\n"))

          :replayed
          (answer w request session id (from request) {"Rep-Evaluation" "replayed"})

          :started
          (do
            (deref (:evaluation/done (:submit/evaluation submitted)) grace-ms nil)
            (answer w request session id (from request)
                    {"Rep-Evaluation" "new"
                     "Rep-Session" (name (:submit/session submitted))})))))))

(defn handle
  "The ring handler."
  [w {:keys [request-method uri] :as request}]
  (let [[root session id & more] (rest (str/split uri #"/"))]
    (cond
      (and (= "/" uri) (= :get request-method))
      (text 200 usage)

      (str/starts-with? uri "/file/")
      (case request-method
        :put (files/store! w request)
        :get (files/fetch w request)
        (text 405 "PUT or GET.\n"))

      (not (and (= "rep" root) session id (not more)))
      (text 404 "Not found. GET / tells how this works.\n")

      (not (authorized? w request))
      (text 401 "Send the header Authorization: Bearer <token>.\n")

      (not (and (parse-uuid session) (parse-uuid id)))
      (text 400 "The session and the evaluation id have to be UUIDs.\n")

      (not= :post request-method)
      (text 405 "POST the code. The same POST again answers the same text.\n")

      :else
      (evaluate w request session id))))

(defn- random-hex
  [length]
  (let [data (byte-array length)]
    (.nextBytes (java.security.SecureRandom.) data)
    (apply str (map #(format "%02x" %) data))))

(defn prepare
  "The system map from the options, the environment and the defaults.

   :rep/dir           the work folder, where the sessions run and files live
   :rep/state-dir     where the evaluations are kept, `.rep` in the work folder
   :rep/token         the bearer token. A random one is made when none is set
   :rep/secret        signs the file URLs, derived from the token when not set
   :rep/public-url    how the outside reaches the server, else from the request
   :rep/prelude       a Clojure file every session loads when it starts, for
                      the functions a host wants to add
   :rep/port :rep/ip  where to listen, 8730 on 127.0.0.1
   :rep/output-limit  the most bytes one evaluation may write
   :rep/max-body      the largest request body, uploads included
   :rep/url-seconds   how long a signed URL holds
   :rep/grace-ms      how long a POST waits for the evaluation before it
                      answers \"running\" and streams"
  [options]
  (let [env (fn [variable]
              (not-empty (System/getenv variable)))
        dir (str (fs/absolutize (or (:rep/dir options) (env "REP_DIR") "work")))
        given (or (:rep/token options) (env "REP_TOKEN"))
        token (or given (random-hex 16))]
    (fs/create-dirs dir)
    (merge {:rep/dir dir
            :rep/state-dir (str (fs/path dir ".rep"))
            :rep/token token
            :rep/token-made? (not given)
            :rep/secret (or (env "REP_SECRET") (files/hmac token "rep file urls"))
            :rep/public-url (env "REP_PUBLIC_URL")
            :rep/prelude (env "REP_PRELUDE")
            :rep/port (or (some-> (env "REP_PORT") parse-long) 8730)
            :rep/ip (or (env "REP_IP") "127.0.0.1")
            :rep/output-limit (* 10 1024 1024)
            :rep/max-body (* 512 1024 1024)
            :rep/url-seconds 900
            :rep/grace-ms 1000
            :rep/lock (Object.)
            :rep/sessions (atom {})
            :rep/evaluations (atom {})}
           (dissoc options :rep/dir :rep/token))))

(defn start!
  "Starts the HTTP server of a prepared system and adds `:rep/server`.
   `:rep/port` becomes the port it listens on, which matters for port 0."
  [{:rep/keys [port ip max-body] :as w}]
  (let [server (http/run-server (fn [request] (handle w request))
                                {:port port
                                 :ip ip
                                 :thread 16
                                 :max-body max-body
                                 :legacy-return-value? false})]
    (assoc w
           :rep/server server
           :rep/port (http/server-port server))))

(defn stop!
  "Stops the HTTP server and ends the processes of the sessions. What
   still ran in them fails, and gets a moment to record that."
  [{:rep/keys [server evaluations] :as w}]
  (when server
    (http/server-stop! server))
  (session/stop-all! w)
  (loop [waited 0]
    (when (and (seq @evaluations) (< waited 3000))
      (Thread/sleep 20)
      (recur (+ waited 20))))
  (dissoc w :rep/server))

(def cli-spec
  {:port {:coerce :long :desc "Port to listen on (8730)"}
   :ip {:desc "Address to listen on (127.0.0.1)"}
   :dir {:desc "Work folder (work)"}
   :public-url {:desc "How the outside reaches the server"}
   :prelude {:desc "A Clojure file every session loads when it starts"}})

(defn -main
  "bb serve [--port 8730] [--ip 127.0.0.1] [--dir work] [--public-url URL] [--prelude FILE]"
  [& args]
  (let [{:keys [port ip dir public-url prelude]} (cli/parse-opts args {:spec cli-spec})
        w (-> (cond-> {}
                port (assoc :rep/port port)
                ip (assoc :rep/ip ip)
                dir (assoc :rep/dir dir)
                public-url (assoc :rep/public-url public-url)
                prelude (assoc :rep/prelude prelude))
              (prepare)
              (start!))]
    (println (str "rep listens on http://" (:rep/ip w) ":" (:rep/port w)))
    (println (str "Work folder: " (:rep/dir w)))
    (when (:rep/token-made? w)
      (println (str "Token: " (:rep/token w))))
    @(promise)))

(comment
  (def w (start! (prepare {:rep/port 8730 :rep/token "dev"})))
  (stop! w)
  ;; curl -s -H 'Authorization: Bearer dev' --data-binary '(+ 1 2)' \
  ;;   http://127.0.0.1:8730/rep/$(uuidgen)/$(uuidgen)
  )
