(ns rep.server
  "REP over HTTP: read, eval, print, and the loop stays with the caller.

     POST /rep                  evaluates the body, answers its output
     PUT  /file/<path>          the body becomes a file of the work folder,
                                with ?offset=<n> one part of a large file
     GET  /file/<path>          answers the file, a part of it with a
                                Range header

   Each needs the header `Authorization: Bearer <token>`. The token is a
   JWT that somebody else signed. The server verifies it against a
   public key and does nothing else about who calls, see `rep.auth`.

   The body of the POST is Clojure code for the session, a babashka
   process of its own. The answer is plain text: what the code printed
   and the value of its last form, with the output of bash as bash wrote
   it. The outcome travels in the headers Rep-Status and Rep-Exit, so the
   text needs no frame around it.

   The session is the token's: its claim `sid`, else `sub`, see
   `rep.auth/session`. A caller that wants another one, or an id of its
   own for an evaluation, names them in the path:

     POST /rep/<session>        a session by its UUID
     POST /rep/<session>/<id>   and an evaluation by its UUID

   Without an id the server makes one. With one the same POST again
   runs nothing, it answers the same text, from byte `?from=` on when
   asked.

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
            [rep.session :as session]
            [rep.token :as token]))

(def text-headers
  {"Content-Type" "text/plain; charset=utf-8"})

(def max-body
  "The largest request body the server takes when the system names no
   `:rep/max-body`: 32 MB. http-kit holds a body in memory before a
   route or a credential is looked at, so this is what anybody who
   reaches the port can make the server hold per connection."
  (* 32 1024 1024))

(defn usage
  "What a GET on / answers, for an agent that only knows the address:
   the protocol in under 40 lines, with the three curl commands."
  [{:rep/keys [shared-image?] :as w}]
  (str/join
   "\n"
   (concat
    ["rep: read, eval, print. You keep the loop."
     ""
     "POST /rep with Clojure code as the body. The answer is plain text: what the code"
     "printed, then the value of its last form, or the error as babashka prints it."
     "Every request carries your token in the header Authorization: Bearer <token>."
     "Keep the token in a variable or a file and never print it."
     ""
     "  curl -s -H \"Authorization: Bearer $REP_TOKEN\" --data-binary @- \"$REP_URL/rep\" <<'EOF'"
     "  (def greeting \"it's \\\"quoted\\\"\")"
     "  (println greeting)"
     "  (sh \"date\")"
     "  EOF"
     ""
     "The code goes through the heredoc as you wrote it, quotes and line breaks included."
     "Your token is one session, a babashka process: what you define stays for your next"
     "request. (sh script) runs bash, and its output is in the answer. Start with (help)."
     ""
     "Files of the work folder, with the same header:"
     ""
     "  curl -s -H \"Authorization: Bearer $REP_TOKEN\" -T clip.mp4 \"$REP_URL/file/in/clip.mp4\""
     "  curl -s -H \"Authorization: Bearer $REP_TOKEN\" -o final.mp4 \"$REP_URL/file/out/final.mp4\""
     ""
     (str "One request carries " (quot (or (:rep/max-body w) max-body) (* 1024 1024)) " MB. A larger file goes up in parts, each a PUT with")
     "?offset=<bytes so far>, and comes down in parts with curl -r <from>-<to>, 8 MB each."
     ""
     "Long work: start it in a future, keep the result in an atom, ask in a later request."
     "A request that takes longer than a second streams its output, and it runs on when the"
     "connection drops. To send a POST again safely, give it two UUIDs of your own:"
     "POST /rep/<session>/<id>. The same POST again runs nothing and answers the same text,"
     "with ?from=<bytes you have> the rest of it. A session you name is not your token's:"
     "what you defined through POST /rep is not in it. POST /rep/<session> works too."
     "The headers Rep-Status (running, done, failed, interrupted) and Rep-Exit (0 or 1)"
     "tell a program the outcome."]
    (when shared-image?
      [""
       "This server runs one shared image: all sessions evaluate in the same babashka process."
       "Every caller sees a definition you make, and another caller can redefine it."])
    [""])))

(defn- text
  ([status body]
   (text status body nil))
  ([status body headers]
   {:status status
    :headers (merge text-headers headers)
    :body body}))

(defn- bearer
  "The token the request carries in its header `Authorization: Bearer
   <token>`, or nil. A token anywhere else, in the query for one, is no
   token."
  [request]
  (some->> (get-in request [:headers "authorization"])
           (re-matches #"(?i)\s*bearer +(\S+)\s*")
           (second)))

(def unauthorized
  "What a request hears whose token `rep.auth/check` does not let in,
   whatever the reason: a caller that probes learns nothing from it.
   The reason goes to the server's own standard error, see `claims`."
  {:status 401
   :headers (assoc text-headers "WWW-Authenticate" "Bearer")
   :body (str "Send the header Authorization: Bearer <token>, with a token this server accepts. "
              "GET / tells how this works.\n")})

(defn- claims
  "The claims of the token that `request` carries, or nil when it is
   refused. Says on standard error that a request was refused and why,
   and nothing of the request: neither its path nor its token."
  [w request]
  (let [{:auth/keys [claims refused]} (auth/check w (bearer request))]
    (when refused
      (binding [*out* *err*]
        (println "rep: a request was refused:" (get auth/reasons refused))))
    claims))

(defn- file
  "Answers a request to `/file/<path>`, which needs the bearer token as
   an evaluation does: a PUT stores the body as the file or as a part
   of it, a GET sends the file or a range of it, a HEAD its length
   alone. See `rep.files/store!` and `rep.files/serve`."
  [w {:keys [request-method] :as request}]
  (let [path (files/requested request)]
    (cond
      (not (claims w request))
      unauthorized

      (not (contains? #{:put :get :head} request-method))
      (text 405 "PUT or GET.\n")

      (nil? path)
      (text 403 "Not a path inside the work folder.\n")

      (= :put request-method)
      (files/store! w request path)

      :else
      (files/serve w request path))))

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
  [{:rep/keys [grace-ms] :as w} request session id claims]
  (let [body (some-> (:body request) (.readAllBytes) (String. "UTF-8"))]
    (if (str/blank? body)
      (text 400 "The body is empty. Send the code to evaluate.\n")
      (let [submitted (evaluation/submit! w {:request/session session
                                             :request/id id
                                             :request/body body
                                             :request/claims claims})]
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

(defn- rep
  "Answers a request to `/rep`, `/rep/<session>` or
   `/rep/<session>/<id>`. A path without a session means the session of
   the token (`rep.auth/session`), and a path without an id gets a
   fresh one, so that only a caller that chose its id can send the
   same POST again."
  [w {:keys [request-method] :as request} session id]
  (let [claims (claims w request)
        session (or session (auth/session claims))]
    (cond
      (not claims)
      unauthorized

      (not session)
      (text 400 (str "This token names no session, it has neither the claim sid nor sub. "
                     "POST to /rep/<session> with a UUID of your own.\n"))

      (not (and (parse-uuid session) (or (nil? id) (parse-uuid id))))
      (text 400 "The session and the evaluation id have to be UUIDs.\n")

      (not= :post request-method)
      (text 405 "POST the code. The same POST again answers the same text.\n")

      :else
      (evaluate w request session (or id (str (random-uuid))) claims))))

(defn rep-path?
  "True when the path `uri` of a request is one of rep's own: `/rep`
   and everything below `/rep/` and `/file/`. A host that owns the HTTP
   server hands such a request to `handle` and answers every other one
   itself. `/` is not among them. `handle` answers it with the usage
   text, but a host has a page of its own there."
  [uri]
  (let [uri (str uri)]
    (or (= "/rep" uri)
        (str/starts-with? uri "/rep/")
        (str/starts-with? uri "/file/"))))

(defn handle
  "The ring handler. It takes the system `w` and a ring request and
   answers these paths, every other one with a 404:

     POST /rep                  evaluates the body in the session of the
                                token, needs the bearer token
     POST /rep/<session>        the same in the session of that UUID
     POST /rep/<session>/<id>   and as the evaluation of that UUID, which
                                the same POST again answers from
     PUT  /file/<path>          stores the body in the work folder, needs
                                the bearer token
     GET  /file/<path>          answers the file, HEAD its length alone,
                                both need the bearer token
     GET  /                     the usage text for an agent

   `start!` puts it behind an HTTP server of rep's own. A host that
   owns the HTTP server calls it from its own handler instead, with a
   system from `prepare`, for the requests that `rep-path?` says are
   rep's. The request needs `:uri`, `:request-method`, `:headers`,
   `:query-string`, `:scheme` and `:body` as an input stream, the way
   http-kit hands them over. An evaluation that takes longer than
   `:rep/grace-ms` answers with an http-kit channel that streams, so the
   host's server has to be http-kit for those.

   A host that wants to know who calls before it hands a request over
   asks `rep.auth/claims` with the system and the token: it answers all
   claims of a token that is let in. The hook `:rep/after-evaluation`
   gets them too."
  [w {:keys [request-method uri] :as request}]
  (let [[root session id & more] (rest (str/split uri #"/"))]
    (cond
      (and (= "/" uri) (= :get request-method))
      (text 200 (usage w))

      (str/starts-with? uri "/file/")
      (file w request)

      (and (= "rep" root) (not more))
      (rep w request session id)

      :else
      (text 404 "Not found. GET / tells how this works.\n"))))

(defn prepare
  "The system map from the options, the environment and the defaults.

   :rep/dir           the work folder, where the sessions run and files live
   :rep/state-dir     where the evaluations are kept, `.rep` in the work folder
   :rep/public-keys   the Ed25519 public keys a token's signature may be
                      from, one text with one or several keys or a vector
                      of such texts, also `REP_PUBLIC_KEYS`. A key is the
                      base64 of its SubjectPublicKeyInfo or its PEM file,
                      see `rep.auth/public-keys`. Without a key every
                      request is refused. A text that is no key throws
   :rep/audience      the name of this machine, which the `aud` of a token
                      has to hold, also `REP_AUDIENCE`. A server with keys
                      and without it throws
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
   :rep/door?         true puts `rep.door` in front of http-kit, so that a
                      plain GET sends a file of any size with its length.
                      Off by default, also `REP_DOOR=1` and `bb serve
                      --door`. The door is a listener of our own in front
                      of every request, a job that usually belongs to a
                      proven server, so it is for whoever needs it. Without
                      it a plain GET sends a file up to 2 GB, a larger one
                      comes in parts with `Range`
   :rep/slice-bytes   how much of a file the door holds at a time while a
                      GET fetches it whole, one megabyte, see `rep.door`
   :rep/grace-ms     how long a POST waits for the evaluation before it
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
                          :evaluation/exit, :evaluation/code and
                          :evaluation/claims, all claims of the token the
                          POST came with"
  [options]
  (let [env (fn [variable]
              (not-empty (System/getenv variable)))
        dir (str (fs/absolutize (or (:rep/dir options) (env "REP_DIR") "work")))
        state-dir (str (or (:rep/state-dir options) (fs/path dir ".rep")))
        public-keys (auth/public-keys (or (:rep/public-keys options) (env "REP_PUBLIC_KEYS") []))
        audience (or (:rep/audience options) (env "REP_AUDIENCE"))]
    (when (and (seq public-keys) (str/blank? audience))
      (throw (ex-info (str "A server with public keys needs the name a token's aud has to hold: "
                           "set :rep/audience or REP_AUDIENCE.")
                      {:rep/error :audience})))
    (fs/create-dirs dir)
    (merge {:rep/dir dir
            :rep/state-dir state-dir
            :rep/public-keys public-keys
            :rep/audience audience
            :rep/prelude (env "REP_PRELUDE")
            :rep/setup-ms session/setup-ms
            :rep/shared-image? (contains? #{"1" "true"} (env "REP_SHARED_IMAGE"))
            :rep/door? (contains? #{"1" "true"} (env "REP_DOOR"))
            :rep/backoff-ms 100
            :rep/backoff-max-ms 5000
            :rep/port (or (some-> (env "REP_PORT") parse-long) 8730)
            :rep/ip (or (env "REP_IP") "127.0.0.1")
            :rep/output-limit (* 10 1024 1024)
            :rep/max-body max-body
            :rep/max-file-mb files/max-file-mb
            :rep/grace-ms 1000
            :rep/keep-days evaluation/keep-days
            :rep/lock (Object.)
            :rep/sweeper (atom nil)
            :rep/sessions (atom {})
            :rep/image (atom {:image/generation 0 :image/seen {}})
            :rep/evaluations (atom {})}
           (dissoc options :rep/dir :rep/state-dir :rep/public-keys :rep/audience))))

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
  "True for the path of a file of the work folder, `/file/<path>`. With
   `:rep/door?` the door answers a GET of such a path itself, so that a
   file of any size comes down in one request."
  [uri]
  (str/starts-with? (str uri) "/file/"))

(defn- listen!
  "Starts what listens on `:rep/ip` and `:rep/port` and answers
   `:rep/server`, http-kit's server, `:rep/port`, the port that took the
   requests of the outside, and with the door `:rep/door`.

   Without `:rep/door?`, the default, that is http-kit alone, and
   nothing of `rep.door` is in the path of a request. With it the port
   belongs to the door and http-kit listens on a port of 127.0.0.1
   behind it."
  [{:rep/keys [port ip max-body door? slice-bytes]} handler]
  (let [options {:thread 16
                 :max-body max-body
                 :legacy-return-value? false}]
    (if door?
      (let [server (http/run-server handler (assoc options :port 0 :ip "127.0.0.1"))
            entrance (door/start! (cond-> {:door/ip ip
                                           :door/port port
                                           :door/inner-port (http/server-port server)
                                           :door/handler handler
                                           :door/own? file-path?}
                                    slice-bytes (assoc :door/slice-bytes slice-bytes)))]
        {:rep/server server
         :rep/door entrance
         :rep/port (:door/port entrance)})
      (let [server (http/run-server handler (assoc options :port port :ip ip))]
        {:rep/server server
         :rep/port (http/server-port server)}))))

(defn start!
  "Starts the HTTP server of a prepared system and adds `:rep/server`.
   `:rep/port` becomes the port it listens on, which matters for port 0.
   The handler and the hooks get the system as this function answers it.
   With `:rep/shared-image?` it also starts the image and waits until
   that runs, see `rep.image/start!`. It deletes the evaluations that
   are older than `:rep/keep-days` and goes on doing so once a day.

   With `:rep/door?` it puts `rep.door` in front of http-kit and adds
   `:rep/door`. The door is off unless asked for, see `prepare`."
  [{:rep/keys [shared-image?] :as w}]
  (let [system (promise)
        w (merge w (listen! w (fn [request] (handle @system request))))]
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
   :prelude {:desc "A Clojure file every session loads when it starts"}
   :class-path {:coerce :string
                :desc "More paths for the classpath of the sessions, separated by commas"}
   :shared-image {:coerce :boolean
                  :desc "All sessions evaluate in one process that starts with the server"}
   :max-body-mb {:coerce :long
                 :desc "The largest request body in megabytes (32)"}
   :door {:coerce :boolean
          :desc "Puts rep's own listener in front of http-kit, for a plain GET of a file above 2 GB"}})

(defn cli-options
  "The options for `prepare` from the arguments of `bb serve`."
  [args]
  (let [{:keys [port ip dir prelude class-path shared-image max-body-mb door]}
        (cli/parse-opts args {:spec cli-spec})]
    (cond-> {}
      port (assoc :rep/port port)
      ip (assoc :rep/ip ip)
      dir (assoc :rep/dir dir)
      prelude (assoc :rep/prelude prelude)
      class-path (assoc :rep/class-path (vec (remove str/blank? (str/split class-path #","))))
      shared-image (assoc :rep/shared-image? true)
      max-body-mb (assoc :rep/max-body (* max-body-mb 1024 1024))
      door (assoc :rep/door? true))))

(def trial-seconds
  "How long the token of a try on a laptop holds: a day."
  86400)

(defn- trial
  "What `bb serve` adds for a try on a laptop, when no public key is
   configured: `:rep/public-keys`, the public half of a key pair made
   here, `:rep/audience`, and `:trial/token`, one token signed with the
   other half, good for `trial-seconds`. The private key is dropped
   when this function returns and nothing is written to disk, so no
   second token can follow and a restart ends this one."
  [{:rep/keys [audience]}]
  (let [{:token/keys [public-key private-key]} (token/key-pair)
        audience (or (not-empty audience) "rep")]
    {:rep/public-keys (auth/public-keys public-key)
     :rep/audience audience
     :trial/token (token/sign {"aud" audience
                               "sub" "laptop"
                               "iat" (auth/now)
                               "exp" (+ (auth/now) trial-seconds)}
                              private-key)}))

(defn local?
  "True when `ip` is an address that only this machine reaches."
  [ip]
  (let [ip (str ip)]
    (boolean (or (contains? #{"localhost" "::1"} ip)
                 (re-matches #"127\.\d+\.\d+\.\d+" ip)))))

(defn- give-up!
  "Ends `bb serve` with `sentence` on standard error."
  [sentence]
  (binding [*out* *err*]
    (println (str "rep: " sentence)))
  (System/exit 1))

(defn -main
  "bb serve [--port 8730] [--ip 127.0.0.1] [--dir work] [--prelude FILE]
            [--class-path PATH,PATH] [--shared-image] [--max-body-mb 32] [--door]

   With `REP_PUBLIC_KEYS` and `REP_AUDIENCE` the server verifies the
   tokens of whoever holds the private key, and makes none itself.
   Without a public key it is a try on a laptop: it prints one token of
   a key pair that lives in this process only, see `trial`. It does
   that on an address of this machine only. A server that forgot its
   key and listens to others would else print a token that works into
   its log, so it does not start."
  [& args]
  (let [prepared (try
                   (prepare (cli-options args))
                   (catch clojure.lang.ExceptionInfo e
                     (if (:rep/error (ex-data e))
                       (give-up! (ex-message e))
                       (throw e))))
        keyless? (empty? (:rep/public-keys prepared))
        _ (when (and keyless? (not (local? (:rep/ip prepared))))
            (give-up! (str "No public key is configured. Set REP_PUBLIC_KEYS and REP_AUDIENCE. Without them rep "
                           "prints a token of its own, and it does that only on 127.0.0.1, for a try on a laptop.")))
        made (when keyless?
               (trial prepared))
        w (start! (merge prepared (dissoc made :trial/token)))
        url (str "http://" (:rep/ip w) ":" (:rep/port w))]
    (println (str "rep listens on " url))
    (println (str "Work folder: " (:rep/dir w)))
    (when (:rep/shared-image? w)
      (println "All sessions share one image."))
    (when (:rep/door? w)
      (println "The door is on: rep's own listener stands in front of http-kit and sends files of any size."))
    (if made
      (do
        (println (str "No public key is configured (REP_PUBLIC_KEYS), so this is a try on a laptop: rep made a key pair "
                      "that lives in this process only and signed one token with it, good for a day. Nothing of it "
                      "is on disk, and a restart ends the token. A server for others gets the public key of whoever "
                      "hands out its tokens."))
        (println (str "export REP_URL=" url))
        (println (str "export REP_TOKEN=" (:trial/token made))))
      (println (str "Callers send a token for the audience " (pr-str (:rep/audience w)) ", signed with the private key of "
                    (if (= 1 (count (:rep/public-keys w)))
                      "the configured public key."
                      (str "one of the " (count (:rep/public-keys w)) " configured public keys.")))))
    @(promise)))

(comment
  (def pair (token/key-pair))
  (def options {:rep/public-keys (:token/public-key pair) :rep/audience "dev"})
  (def w (start! (prepare (assoc options :rep/port 8730))))
  (def bearer-token (token/sign {"aud" "dev" "sub" "me" "exp" (+ (auth/now) 3600)} (:token/private-key pair)))
  (stop! w)

  (cli-options ["--port" "8730" "--shared-image" "--class-path" "shop/src,shop/resources"])

  ;; a host with an HTTP server of its own mounts the handler
  (def w (image/start! (prepare (assoc options :rep/shared-image? true))))
  (defn host-handler
    [request]
    (if (rep-path? (:uri request))
      (handle w request)
      {:status 200 :body "the host's own page"}))
  (def server (http/run-server host-handler {:port 8731 :legacy-return-value? false}))
  (http/server-stop! server)
  (stop! w))
