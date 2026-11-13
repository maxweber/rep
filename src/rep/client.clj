(ns rep.client
  "The command line client. It reads code from standard input, so the
   code needs no quoting on the way, prints the answer as it comes and
   ends with 0 when the code ran through and 1 when it failed.

     rep <<'EOF'
     (+ 1 2)
     EOF

   It makes the evaluation id itself and sends the same id when it has to
   ask again, after a broken connection or for the rest of a long answer.
   The server runs the code once.

   `rep login <code>` exchanges a login code for a token and keeps the
   token in the user's config folder, so that it is neither printed nor
   left in the current folder.

   `rep upload LOCAL PATH` and `rep download PATH LOCAL` move a file
   into the server's work folder and out of it. They ask the session
   for a signed URL and send or fetch the file in parts of 8 MB,
   because a server holds a request body in memory and takes no large
   one. A signed URL that runs out in the middle is replaced by a fresh
   one, and `--resume` goes on with a transfer that was cut."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.http-client :as http]
            [cheshire.core :as json]
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

(defn token-file
  "Where the token for the server at `url` is kept: a file named after the
   server's host and port in `$XDG_CONFIG_HOME/rep`, by default
   `~/.config/rep`."
  [url]
  (let [uri (java.net.URI. url)
        port (cond
               (pos? (.getPort uri)) (.getPort uri)
               (= "https" (.getScheme uri)) 443
               :else 80)
        config (or (not-empty (System/getenv "XDG_CONFIG_HOME"))
                   (fs/path (fs/home) ".config"))]
    (fs/path config "rep" (str (.getHost uri) "-" port ".token"))))

(defn- stored-token
  [url]
  (let [file (token-file url)]
    (when (fs/exists? file)
      (not-empty (str/trim (slurp (fs/file file)))))))

(defn- settings
  "The server's address and the token for it: the option, `REP_TOKEN`, or
   the one `rep login` stored."
  [{:keys [url token]}]
  (let [url (str/replace (or url (System/getenv "REP_URL") "http://127.0.0.1:8730") #"/+$" "")]
    {:client/url url
     :client/token (or token
                       (not-empty (System/getenv "REP_TOKEN"))
                       (stored-token url))}))

(defn- expiry
  "The day a token expires, read from its payload."
  [token]
  (try
    (let [payload (second (str/split token #"\."))
          claims (json/parse-string (String. (.decode (java.util.Base64/getUrlDecoder) payload) "UTF-8"))]
      (subs (str (java.time.Instant/ofEpochSecond (get claims "exp"))) 0 10))
    (catch Exception _ nil)))

(defn login
  "Exchanges the login code for a token and stores it, readable by the
   user only. Says that it worked and until when, never the token.
   Answers the exit code."
  [{:client/keys [url]} code]
  (let [{:keys [status body]} (http/post (str url "/login") {:body (str code) :throw false})]
    (if (= 200 status)
      (let [file (token-file url)]
        (fs/create-dirs (fs/parent file))
        (spit (fs/file file) "")
        (fs/set-posix-file-permissions file "rw-------")
        (spit (fs/file file) body)
        (println (str "Logged in to " url
                      (when-let [day (expiry body)]
                        (str " until " day))
                      "."))
        0)
      (do
        (note (str/trim (str body)))
        2))))

(defn logout
  "Ends the stored token at the server and deletes its file."
  [{:client/keys [url]}]
  (let [token (stored-token url)]
    (when token
      (try
        (http/post (str url "/logout") {:headers {"Authorization" (str "Bearer " token)}
                                        :throw false})
        (catch java.io.IOException _ nil)))
    (fs/delete-if-exists (token-file url))
    (println (if token "Logged out." "Not logged in."))
    0))

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
  "Copies the answer `in` to the stream `out` as it arrives. Answers how
   many bytes came, and the error when the connection broke on the way."
  [in out]
  (let [buffer (byte-array 8192)]
    (loop [total 0]
      (let [n (try
                (.read in buffer)
                (catch java.io.IOException e e))]
        (cond
          (instance? Throwable n) {:bytes total :broken n}
          (neg? n) {:bytes total}
          :else (do
                  (.write out buffer 0 n)
                  (.flush out)
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
   where it stopped. The output goes to the stream `out`, to standard
   output without one. Adds `:client/status` and `:client/exit`."
  ([w id code]
   (follow w id code System/out))
  ([w id code out]
   (loop [response (attempt w id code 0)
          received 0
          rounds 0]
     (let [{:keys [status headers body]} response]
       (if (not= 200 status)
         (do
           (note (str/trim (slurp body)))
           (assoc w :client/status "refused" :client/exit 2))
         (let [{:keys [bytes broken]} (copy! body out)
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
                    (inc rounds)))))))))

(def part-bytes
  "How much of a file one request of `upload!` and `download!` carries:
   eight megabytes, well below the largest body a server takes."
  (* 8 1024 1024))

(def part-tries
  "How often a part is sent or asked for before the client gives up."
  4)

(def url-tries
  "How often in a row a transfer asks the session for a fresh signed URL
   after the server refused the one it has. A signed URL holds for some
   minutes and a long transfer outlives it, so a refusal in the middle
   is no reason to stop. Several in a row are."
  3)

(defn- failure
  "The exception that ends an upload or a download, with a sentence for
   the person at the terminal."
  [sentence]
  (ex-info sentence {:client/failed? true}))

(defn- ask
  "Evaluates `code` in the session and answers what it printed,
   trimmed. Throws with `problem` and what the session said when the
   evaluation failed."
  [w code problem]
  (let [out (java.io.ByteArrayOutputStream.)
        {:client/keys [exit]} (follow w (str (random-uuid)) code out)
        answer (str/trim (.toString out "UTF-8"))]
    (when-not (zero? exit)
      (throw (failure (str problem (when (seq answer) (str ": " answer))))))
    answer))

(defn- signed-url
  "Asks the session for a signed URL: evaluates `(rep.prelude/<function>
   path)` and answers the URL with this client's own address of the
   server in front, the one it knows it can reach. Throws when the
   session gave none."
  [w function path]
  (let [problem (str "The session gave no URL for " (pr-str (str path)))
        answer (ask w (str "(print (rep.prelude/" function " " (pr-str (str path)) "))") problem)
        index (str/index-of answer "/file/")]
    (when-not index
      (throw (failure (str problem (when (seq answer) (str ": " answer))))))
    (str (:client/url w) (subs answer index))))

(defn- remote-size
  "Asks the session how many bytes the file at `path` of the work
   folder holds, 0 when there is none."
  [w path]
  (let [problem (str "The session did not tell the size of " (pr-str (str path)))
        answer (ask w (str "(print (rep.prelude/file-size " (pr-str (str path)) "))") problem)]
    (or (parse-long answer)
        (throw (failure (str problem ": " answer))))))

(defn- with-tries
  "Calls `request` until it answers. A connection that fails is tried
   again a few times, after a growing pause."
  [request]
  (loop [tries 1]
    (let [response (try
                     (request)
                     (catch java.io.IOException e e))]
      (if (instance? Throwable response)
        (if (< tries part-tries)
          (do
            (Thread/sleep (* 500 tries))
            (recur (inc tries)))
          (throw (failure (str "The server does not answer: " (ex-message response)))))
        response))))

(defn- held
  "How many bytes the server says the file holds, from the header of
   its answer."
  [response]
  (some-> (get (:headers response) "rep-file-size") parse-long))

(defn- read-part
  "At most `part-bytes` of the open file `in` from `offset` on."
  [in offset]
  (let [buffer (byte-array (min part-bytes (max 0 (- (.length in) offset))))]
    (.seek in offset)
    (.readFully in buffer)
    buffer))

(defn- put-parts!
  "Sends the open file `in` from byte `begin` on as the parts of the
   upload `id`, each to a signed URL that `sign` answers. `takeover?`
   says that the first part takes over a file another upload began.
   Answers the bytes the server holds at the end."
  [in sign id begin takeover?]
  (let [length (.length in)]
    (loop [url (sign)
           offset begin
           refused 0
           takeover? takeover?]
      (let [part (read-part in offset)
            end (+ offset (alength part))
            address (str url "&offset=" offset "&upload=" id (when takeover? "&resume=true"))
            response (with-tries #(http/put address {:body part :throw false}))
            status (:status response)
            size (held response)]
        (cond
          (and (= 201 status) (= end size))
          (if (< end length)
            (recur url end 0 false)
            size)

          ;; the part arrived and its answer got lost
          (and (= 409 status) size (< offset size) (<= size length))
          (if (< size length)
            (recur url size 0 false)
            size)

          ;; the signed URL ran out in the middle
          (and (= 403 status) (< refused url-tries))
          (recur (sign) offset (inc refused) takeover?)

          :else
          (throw (failure (str "The upload stopped at byte " offset " (HTTP " status "): "
                               (str/trim (str (:body response)))))))))))

(defn upload!
  "Sends the local file `local` to `path` of the server's work folder,
   through signed URLs the session hands out, in parts of `part-bytes`.
   Answers the bytes the file holds at the end. Throws with a sentence
   when it does not work.

   - A part whose answer got lost is not sent twice: the server says
     how many bytes it holds, and the upload goes on from there.
   - A signed URL that runs out in the middle is no end: the client asks
     the session for a fresh one and goes on at the same offset.
   - Every part names this upload with an id of its own, so a second
     upload of the same path is told that another one writes the file.
   - With `:resume? true` it asks the server how many bytes the file
     has and sends the rest, for an upload that was cut. It takes the
     file over from the upload that began it."
  ([w local path]
   (upload! w local path nil))
  ([w local path {:keys [resume?]}]
   (when-not (fs/regular-file? local)
     (throw (failure (str "There is no file " local "."))))
   (with-open [in (java.io.RandomAccessFile. (str local) "r")]
     (let [length (.length in)
           begin (if resume? (remote-size w path) 0)]
       (cond
         (< length begin)
         (throw (failure (str "The server holds " begin " bytes of " path ", more than the " length
                              " bytes of " local ". That is another file. Upload it without --resume.")))

         (and (pos? begin) (= begin length))
         length

         :else
         (put-parts! in #(signed-url w "upload-url" path) (str (random-uuid)) begin (pos? begin)))))))

(defn- total
  "The size of the whole file, from the `Content-Range` of a part or of
   a 416."
  [response]
  (some->> (get (:headers response) "content-range")
           (re-find #"/(\d+)$")
           (second)
           (parse-long)))

(defn- fetch-parts!
  "Writes the file behind a signed URL to the stream `out` from byte
   `begin` on, one range of `part-bytes` after the other. `sign`
   answers the URL, and a fresh one when the server refuses the one in
   use. Answers the byte the file ends at."
  [sign out begin]
  (loop [url (sign)
         offset begin
         refused 0]
    (let [range (str "bytes=" offset "-" (dec (+ offset part-bytes)))
          {:keys [status body] :as response} (with-tries #(http/get url {:headers {"Range" range}
                                                                         :as :bytes
                                                                         :throw false}))
          stop (fn []
                 (failure (str "The download stopped at byte " offset " (HTTP " status "): "
                               (str/trim (String. ^bytes body "UTF-8")))))]
      (cond
        (= 206 status)
        (let [end (+ offset (alength body))]
          (.write out ^bytes body)
          (if (and (pos? (alength body)) (< end (or (total response) 0)))
            (recur url end 0)
            end))

        (and (= 200 status) (zero? offset))
        (do
          (.write out ^bytes body)
          (alength body))

        ;; nothing is left to fetch, unless the local part is longer than the file
        (and (= 416 status) (<= offset (or (total response) offset)))
        offset

        (and (= 403 status) (< refused url-tries))
        (recur (sign) offset (inc refused))

        :else
        (throw (stop))))))

(defn download!
  "Fetches `path` of the server's work folder into the local file
   `local`, through signed URLs the session hands out, in parts of
   `part-bytes`. Answers the bytes the file has. Throws with a sentence
   when it does not work.

   The parts go to `<local>.part`, which becomes the file when all of
   them are there, so a download that breaks leaves an earlier `local`
   as it was and the part file with what came. A signed URL that runs
   out in the middle is no end: the client asks for a fresh one and
   goes on at the same offset.

   With `:resume? true` it goes on from the size of the part file an
   earlier download left. Without a part file it takes `local` itself
   for the part that came so far."
  ([w path local]
   (download! w path local nil))
  ([w path local {:keys [resume?]}]
   (let [part (str local ".part")]
     (some-> (fs/parent (fs/absolutize local)) (fs/create-dirs))
     (when (and resume? (not (fs/exists? part)) (fs/regular-file? local))
       (fs/move local part))
     (let [begin (if (and resume? (fs/regular-file? part)) (fs/size part) 0)]
       (try
         (let [bytes (with-open [out (java.io.FileOutputStream. part (pos? begin))]
                       (fetch-parts! #(signed-url w "download-url" path) out begin))]
           (fs/move part local {:replace-existing true})
           bytes)
         (catch clojure.lang.ExceptionInfo e
           (when (and (fs/exists? part) (zero? (fs/size part)))
             (fs/delete part))
           (throw e)))))))

(defn- moved
  "Runs the upload or download `move`, says what happened and answers
   the exit code."
  [move sentence]
  (try
    (println (sentence (move)))
    0
    (catch clojure.lang.ExceptionInfo e
      (if (:client/failed? (ex-data e))
        (do
          (note (str "rep: " (ex-message e)))
          1)
        (throw e)))))

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
   :token {:desc "Bearer token (REP_TOKEN, else the one rep login stored)"}
   :wait {:coerce :long :desc "Stop following after this many seconds, exit 124"}
   :resume {:coerce :boolean :desc "upload, download: go on where an earlier transfer of the file stopped"}})

(def help
  (str/join
   "\n"
   ["rep [--id ID] [--wait SECONDS] < code   evaluates standard input, prints the answer"
    "rep upload [--resume] LOCAL PATH        sends a local file into the work folder, in parts"
    "rep download [--resume] PATH LOCAL      fetches a file of the work folder, in parts"
    "rep login CODE                          exchanges a login code for a token and stores it"
    "rep logout                              ends the stored token"
    "rep session                             prints the session id"
    ""
    (cli/format-opts {:spec cli-spec})]))

(defn- in-session
  "`w` with the session id, for a command that evaluates. Ends the
   client with 2 when there is no token to send."
  [w opts]
  (when-not (:client/token w)
    (note "rep: no token. Log in with rep login <code>, or set REP_TOKEN.")
    (System/exit 2))
  (assoc w :client/session (session (:session-id opts))))

(defn -main
  [& args]
  (let [{:keys [opts args]} (cli/parse-args args {:spec cli-spec})
        [command argument second-argument] args]
    (if (:help opts)
      (println help)
      (let [w (settings opts)]
        (case command
          "login" (System/exit (login w argument))
          "logout" (System/exit (logout w))
          "session" (println (session (:session-id opts)))
          "upload" (if (and argument second-argument)
                     (let [w (in-session w opts)]
                       (System/exit (moved #(upload! w argument second-argument {:resume? (:resume opts)})
                                           #(str "Stored " second-argument " (" % " bytes)."))))
                     (do
                       (note "rep upload [--resume] LOCAL PATH")
                       (System/exit 2)))
          "download" (if (and argument second-argument)
                       (let [w (in-session w opts)]
                         (System/exit (moved #(download! w argument second-argument {:resume? (:resume opts)})
                                             #(str "Fetched " argument " into " second-argument " (" % " bytes)."))))
                       (do
                         (note "rep download [--resume] PATH LOCAL")
                         (System/exit 2)))
          nil (let [w (in-session w opts)
                    id (or (:id opts) (str (random-uuid)))
                    code (slurp *in*)]
                (finish (within (:wait opts) id #(follow w id code))))
          (do
            (note help)
            (System/exit 2)))))))

(comment
  (def w (assoc (settings {}) :client/session (session nil)))
  (follow w (str (random-uuid)) "(+ 1 2)")
  (upload! w "clip.mp4" "in/clip.mp4")
  (upload! w "clip.mp4" "in/clip.mp4" {:resume? true})
  (download! w "out/final.mp4" "final.mp4")
  (download! w "out/final.mp4" "final.mp4" {:resume? true})
  (token-file "https://machine.example"))
