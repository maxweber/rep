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
   answers the same text, from byte `?from=` on when asked.

     POST /login    body = a login code, answer = a token (see `rep.auth`)
     POST /logout   ends the token it is sent with"
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [rep.auth :as auth]
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
    "If you were given a login code, exchange it once for a token. Keep the token in a file and do not print it:"
    "  curl -s -X POST --data-binary \"$CODE\" \"$REP_URL/login\" -o ~/.rep-token"
    "  then send -H \"Authorization: Bearer $(cat ~/.rep-token)\""
    "POST /logout with that header ends the token."
    ""
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

(defn- bearer
  "The token the request carries, or nil."
  [request]
  (some-> (get-in request [:headers "authorization"])
          (str/replace-first #"(?i)^bearer\s+" "")
          (str/trim)
          (not-empty)))

(defn- authorized?
  "True for the static token, when one is configured, and for a token the
   server gave out that is still good."
  [{:rep/keys [token] :as w} request]
  (let [given (bearer request)]
    (boolean
     (and given
          (or (and token
                   (java.security.MessageDigest/isEqual (.getBytes given "UTF-8")
                                                        (.getBytes (str token) "UTF-8")))
              (auth/claims w given))))))

(defn- login
  "Answers /login. The body is a login code, the answer its token and
   nothing else, so that a caller can write it straight into a file. Only
   a POST looks at the code. A link preview that GETs the address burns
   nothing."
  [w {:keys [request-method body]}]
  (if (not= :post request-method)
    (text 405 "POST the login code as the body.\n")
    (if-let [token (some->> body
                            (.readAllBytes)
                            (String.)
                            (not-empty)
                            (auth/exchange! w))]
      (text 200 token)
      (text 401 "This code is unknown, used or expired. Ask for a new one.\n"))))

(defn- logout
  "Answers /logout: ends the token the request was sent with."
  [w {:keys [request-method] :as request}]
  (let [claims (some->> (bearer request) (auth/claims w))]
    (cond
      (not= :post request-method)
      (text 405 "POST with the token as the bearer.\n")

      claims
      (do
        (auth/revoke! w (get claims "jti"))
        (text 200 "Logged out.\n"))

      :else
      (text 401 "Send the token you want to end as Authorization: Bearer <token>.\n"))))

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

      (= "/login" uri)
      (login w request)

      (= "/logout" uri)
      (logout w request)

      (not (and (= "rep" root) session id (not more)))
      (text 404 "Not found. GET / tells how this works.\n")

      (not (authorized? w request))
      (text 401 "Send the header Authorization: Bearer <token>. GET / tells how to get one.\n")

      (not (and (parse-uuid session) (parse-uuid id)))
      (text 400 "The session and the evaluation id have to be UUIDs.\n")

      (not= :post request-method)
      (text 405 "POST the code. The same POST again answers the same text.\n")

      :else
      (evaluate w request session id))))

(defn prepare
  "The system map from the options, the environment and the defaults.

   :rep/dir           the work folder, where the sessions run and files live
   :rep/state-dir     where the evaluations are kept, `.rep` in the work folder
   :rep/token         a static bearer token, for tests and automation. Without
                      one, callers log in with a code (see `rep.auth`)
   :rep/secret        the key that signs the tokens, from `REP_SECRET` or kept
                      in the state folder. `:rep/file-secret`, which signs the
                      file URLs and is all a session gets, is derived from it
   :rep/code-minutes  how long a login code holds, 10
   :rep/token-days    how long a token holds, 30
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
        state-dir (str (or (:rep/state-dir options) (fs/path dir ".rep")))
        _ (fs/create-dirs dir)
        secret (or (:rep/secret options) (env "REP_SECRET") (auth/secret! state-dir))]
    (merge {:rep/dir dir
            :rep/state-dir state-dir
            :rep/token (or (:rep/token options) (env "REP_TOKEN"))
            :rep/secret secret
            :rep/file-secret (auth/file-secret secret)
            :rep/code-minutes 10
            :rep/token-days 30
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
           (dissoc options :rep/dir :rep/state-dir :rep/token :rep/secret))))

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
    (if (:rep/token w)
      (println "Callers send the static token of REP_TOKEN.")
      (let [code (auth/mint-code! w)]
        (println (str "Login code, good once and for " (:rep/code-minutes w) " minutes: " code))
        (println (str "  rep login " code))
        (println "bb code makes another one.")))
    @(promise)))

(comment
  (def w (start! (prepare {:rep/port 8730 :rep/token "dev"})))
  (stop! w)
  ;; curl -s -H 'Authorization: Bearer dev' --data-binary '(+ 1 2)' \
  ;;   http://127.0.0.1:8730/rep/$(uuidgen)/$(uuidgen)
  )
