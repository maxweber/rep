(ns rep.server
  "REP over HTTP: read, eval, print, and the loop stays with the caller.

     POST /rep/<session>/<id>   evaluates the body, answers its output
     PUT  /file/<path>?...      a signed upload into the work folder,
                                with &offset=<n> one part of a large file
     GET  /file/<path>?...      a signed download out of it, a part of
                                it with a Range header

   The body of the POST is Clojure code for the session, a babashka
   process of its own. The answer is plain text: what the code printed
   and the value of its last form, with the output of bash as bash wrote
   it. The outcome travels in the headers Rep-Status and Rep-Exit, so the
   text needs no frame around it. The same POST again runs nothing, it
   answers the same text, from byte `?from=` on when asked.

     POST /login    body = a login code, answer = a token (see `rep.auth`)
     POST /logout   ends the token it is sent with

   With `:rep/shared-image?` all sessions evaluate in one babashka
   process that starts with the server, see `rep.image`."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [rep.auth :as auth]
            [rep.door :as door]
            [rep.evaluation :as evaluation]
            [rep.files :as files]
            [rep.image :as image]
            [rep.output :as output]
            [rep.session :as session]))

(def text-headers
  {"Content-Type" "text/plain; charset=utf-8"})

(def max-body
  "The largest request body the server takes when the system names no
   `:rep/max-body`: 32 MB. http-kit holds a body in memory before a
   route or a credential is looked at, so this is what anybody who
   reaches the port can make the server hold per connection."
  (* 32 1024 1024))

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

(def shared-usage
  "What a GET on / adds when all sessions share one image."
  (str/join
   "\n"
   ["This server runs one shared image. All session uuids evaluate in the same babashka process."
    "Every caller sees a definition you make, and another caller can redefine it."
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

(defn rep-path?
  "True when the path `uri` of a request is one of rep's own: `/login`,
   `/logout` and everything below `/rep/` and `/file/`. A host that owns
   the HTTP server hands such a request to `handle` and answers every
   other one itself. `/` is not among them. `handle` answers it with the
   usage text, but a host has a page of its own there."
  [uri]
  (let [uri (str uri)]
    (or (contains? #{"/login" "/logout"} uri)
        (str/starts-with? uri "/rep/")
        (str/starts-with? uri "/file/"))))

(defn handle
  "The ring handler. It takes the system `w` and a ring request and
   answers these paths, every other one with a 404:

     POST /rep/<session>/<id>   evaluates the body, needs the bearer token
     PUT  /file/<path>?...      a signed upload into the work folder
     GET  /file/<path>?...      a signed download out of it, HEAD for
                                its length alone
     POST /login                exchanges a login code for a token
     POST /logout               ends the token it is sent with
     GET  /                     the usage text for an agent

   `start!` puts it behind an HTTP server of rep's own. A host that
   owns the HTTP server calls it from its own handler instead, with a
   system from `prepare`, for the requests that `rep-path?` says are
   rep's. The request needs `:uri`, `:request-method`, `:headers`,
   `:query-string`, `:scheme` and `:body` as an input stream, the way
   http-kit hands them over. An evaluation that takes longer than
   `:rep/grace-ms` answers with an http-kit channel that streams, so the
   host's server has to be http-kit for those."
  [{:rep/keys [shared-image?] :as w} {:keys [request-method uri] :as request}]
  (let [[root session id & more] (rest (str/split uri #"/"))]
    (cond
      (and (= "/" uri) (= :get request-method))
      (text 200 (cond-> usage
                  shared-image? (str shared-usage)))

      (str/starts-with? uri "/file/")
      (case request-method
        :put (files/store! w request)
        (:get :head) (files/fetch w request)
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
   :rep/setup-ms      how long the setup of a fresh process may take, the
                      prelude included, 60000. Past it the process is ended
                      and the start counts as failed
   :rep/class-path    a vector of paths that the session's babashka gets on
                      its classpath, after the server's own. On the command
                      line `--class-path a,b`
   :rep/port :rep/ip  where to listen, 8730 on 127.0.0.1
   :rep/output-limit  the most bytes one evaluation may write
   :rep/max-body      the largest request body in bytes, 32 MB. The server
                      holds a body in memory before it looks at a route or
                      a credential, and refuses a larger one with a 413. A
                      larger file comes in parts, see `rep.files/store!`
   :rep/max-file-mb   how many megabytes a file may grow to through its
                      parts, 4096. A part that would pass that is a 413
   :rep/slice-bytes   how much of a file the door holds at a time while a
                      GET fetches it whole, one megabyte, see `rep.door`
   :rep/url-seconds   how long a signed URL holds
   :rep/grace-ms      how long a POST waits for the evaluation before it
                      answers \"running\" and streams
   :rep/keep-days     how many days the request, the output and the outcome
                      of an evaluation stay on disk, 7. The server deletes
                      older ones when it starts and once a day. 0 keeps
                      them for good

   One shared image, see `rep.image`:

   :rep/shared-image?   true lets all sessions evaluate in one process. It
                        starts with the server, and the server starts it
                        again whenever it ends. Also `REP_SHARED_IMAGE=1`
   :rep/backoff-ms      the pause before the next start of an image that
                        failed to start or ended at once, 100. It doubles
                        with every further failure
   :rep/backoff-max-ms  the longest such pause, 5000

   Hooks. Both run in the server's process. The server prints what a hook
   throws to standard error, and it goes no further:

   :rep/on-image-start    (fn [w image]) after every start of the shared
                          image that worked, its setup code included. `image`
                          has :image/generation, :image/pid, :image/port and
                          :image/start-ms. Requests already run while it runs,
                          the supervisor waits for it
   :rep/after-evaluation  (fn [w evaluation]) after every evaluation of a
                          POST, with or without the shared image. `evaluation`
                          has :evaluation/session, :evaluation/id,
                          :evaluation/state (:done or :failed),
                          :evaluation/exit and :evaluation/code"
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
            :rep/setup-ms session/setup-ms
            :rep/shared-image? (contains? #{"1" "true"} (env "REP_SHARED_IMAGE"))
            :rep/backoff-ms 100
            :rep/backoff-max-ms 5000
            :rep/port (or (some-> (env "REP_PORT") parse-long) 8730)
            :rep/ip (or (env "REP_IP") "127.0.0.1")
            :rep/output-limit (* 10 1024 1024)
            :rep/max-body max-body
            :rep/max-file-mb files/max-file-mb
            :rep/url-seconds 900
            :rep/grace-ms 1000
            :rep/keep-days evaluation/keep-days
            :rep/lock (Object.)
            :rep/sweeper (atom nil)
            :rep/sessions (atom {})
            :rep/image (atom {:image/generation 0 :image/seen {}})
            :rep/evaluations (atom {})}
           (dissoc options :rep/dir :rep/state-dir :rep/token :rep/secret))))

(def sweep-ms
  "How long the sweeper waits between two sweeps: a day."
  (* 24 60 60 1000))

(defn- sweep!
  "One sweep of the stored evaluations and of the records of old
   uploads. A sweep that breaks says so on standard error and takes
   nothing down with it."
  [w]
  (try
    (evaluation/sweep! w)
    (files/forget-uploads! w)
    (catch Throwable e
      (binding [*out* *err*]
        (println "rep: could not delete old evaluations:" (or (ex-message e) (.getName (class e))))))))

(defn start-sweeper!
  "Deletes the evaluations that are older than `:rep/keep-days` now and
   then once a day, on a thread of its own, until `stop!`. `start!` calls
   this. A host that mounts `handle` in its own HTTP server calls it on
   the prepared system itself. A second call replaces the thread of the
   first. Answers `w`."
  [{:rep/keys [sweeper] :as w}]
  (let [stop (promise)
        thread (doto (Thread. (fn []
                                (loop []
                                  (sweep! w)
                                  (when (= ::again (deref stop sweep-ms ::again))
                                    (recur)))))
                 (.setDaemon true)
                 (.start))
        [earlier _] (reset-vals! sweeper {:sweeper/stop stop :sweeper/thread thread})]
    (some-> earlier :sweeper/stop (deliver true))
    w))

(defn- stop-sweeper!
  [{:rep/keys [sweeper]}]
  (when sweeper
    (let [[running _] (reset-vals! sweeper nil)]
      (some-> running :sweeper/stop (deliver true)))))

(defn file-path?
  "True for the path of a signed file URL. `rep.door` answers a GET of
   such a path itself, so that a file of any size comes down in one
   request."
  [uri]
  (str/starts-with? (str uri) "/file/"))

(defn start!
  "Starts the HTTP server of a prepared system and adds `:rep/server`
   and `:rep/door`. `:rep/port` becomes the port it listens on, which
   matters for port 0. The handler and the hooks get the system as this
   function answers it. With `:rep/shared-image?` it also starts the
   image and waits until that runs, see `rep.image/start!`. It deletes
   the evaluations that are older than `:rep/keep-days` and goes on
   doing so once a day.

   The port belongs to the door, `rep.door`, and http-kit listens on a
   port of 127.0.0.1 behind it. The door answers a GET of a file itself
   and passes every other request on, because http-kit cannot send a
   file above 2 GB with its length."
  [{:rep/keys [port ip max-body shared-image? slice-bytes] :as w}]
  (let [system (promise)
        handler (fn [request] (handle @system request))
        server (http/run-server handler
                                {:port 0
                                 :ip "127.0.0.1"
                                 :thread 16
                                 :max-body max-body
                                 :legacy-return-value? false})
        entrance (door/start! (cond-> {:door/ip ip
                                       :door/port port
                                       :door/inner-port (http/server-port server)
                                       :door/handler handler
                                       :door/own? file-path?}
                                slice-bytes (assoc :door/slice-bytes slice-bytes)))
        w (assoc w
                 :rep/server server
                 :rep/door entrance
                 :rep/port (:door/port entrance))]
    (deliver system w)
    (start-sweeper! w)
    (cond-> w
      shared-image? (image/start!))))

(defn stop!
  "Stops the HTTP server, when the system has one, and ends the processes
   of the sessions and the shared image with its supervision. What still
   ran in them fails, and gets a moment to record that. Ends the daily
   sweep as well."
  [{:rep/keys [server door evaluations] :as w}]
  (when door
    (door/stop! door))
  (when server
    (http/server-stop! server))
  (stop-sweeper! w)
  (session/stop-all! w)
  (image/stop! w)
  (loop [waited 0]
    (when (and (seq @evaluations) (< waited 3000))
      (Thread/sleep 20)
      (recur (+ waited 20))))
  (dissoc w :rep/server :rep/door))

(def cli-spec
  {:port {:coerce :long :desc "Port to listen on (8730)"}
   :ip {:desc "Address to listen on (127.0.0.1)"}
   :dir {:desc "Work folder (work)"}
   :public-url {:desc "How the outside reaches the server"}
   :prelude {:desc "A Clojure file every session loads when it starts"}
   :class-path {:coerce :string
                :desc "More paths for the classpath of the sessions, separated by commas"}
   :shared-image {:coerce :boolean
                  :desc "All sessions evaluate in one process that starts with the server"}
   :max-body-mb {:coerce :long
                 :desc "The largest request body in megabytes (32)"}})

(defn cli-options
  "The options for `prepare` from the arguments of `bb serve`."
  [args]
  (let [{:keys [port ip dir public-url prelude class-path shared-image max-body-mb]}
        (cli/parse-opts args {:spec cli-spec})]
    (cond-> {}
      port (assoc :rep/port port)
      ip (assoc :rep/ip ip)
      dir (assoc :rep/dir dir)
      public-url (assoc :rep/public-url public-url)
      prelude (assoc :rep/prelude prelude)
      class-path (assoc :rep/class-path (vec (remove str/blank? (str/split class-path #","))))
      shared-image (assoc :rep/shared-image? true)
      max-body-mb (assoc :rep/max-body (* max-body-mb 1024 1024)))))

(defn -main
  "bb serve [--port 8730] [--ip 127.0.0.1] [--dir work] [--public-url URL] [--prelude FILE]
            [--class-path PATH,PATH] [--shared-image] [--max-body-mb 32]"
  [& args]
  (let [w (-> (cli-options args)
              (prepare)
              (start!))]
    (println (str "rep listens on http://" (:rep/ip w) ":" (:rep/port w)))
    (println (str "Work folder: " (:rep/dir w)))
    (when (:rep/shared-image? w)
      (println "All sessions share one image."))
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

  (cli-options ["--port" "8730" "--shared-image" "--class-path" "shop/src,shop/resources"])

  ;; a host with an HTTP server of its own mounts the handler
  (def w (image/start! (prepare {:rep/token "dev" :rep/shared-image? true})))
  (defn host-handler
    [request]
    (if (rep-path? (:uri request))
      (handle w request)
      {:status 200 :body "the host's own page"}))
  (def server (http/run-server host-handler {:port 8731 :legacy-return-value? false}))
  (http/server-stop! server)
  (stop! w))
