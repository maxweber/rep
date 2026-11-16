(ns rep.prelude
  "What REP adds to a session. A session is a babashka process of its
   own, so files, futures, atoms and every babashka namespace are there
   already. This namespace adds bash with its output in the answer and
   signed URLs for files. The server loads it when it starts the process
   and refers `sh`, `upload-url`, `download-url` and `help` into `user`."
  (:require [babashka.process :as process]
            [rep.files :as files]))

(defonce ^{:doc "What the server told this session: `:secret` signs the
  file URLs and nothing else, `:base-url` is the server's public URL when it
  has one, `:url-seconds` how long a URL holds, `:parent` the server's
  process id, `:shared-image?` is true in the one process that all sessions
  share."}
  config
  (atom {}))

(def ^:dynamic *base-url*
  "Where the caller of the evaluation that runs right now reached the
   server. The server binds it around the code of every request, so in
   a process that several callers share each one gets URLs for its own
   address. A future that the code starts keeps the binding."
  nil)

(defn- parent-alive?
  [pid]
  (boolean (some-> (java.lang.ProcessHandle/of pid)
                   (.orElse nil)
                   (.isAlive))))

(defn- watch-parent!
  "Ends this process when the server that started it is gone, so that a
   killed server leaves no sessions behind."
  [pid]
  (future
    (loop []
      (Thread/sleep 2000)
      (if (parent-alive? pid)
        (recur)
        (System/exit 0)))))

(defn configure!
  "Called by the server once, right after the process started."
  [{:keys [parent] :as options}]
  (when (and parent (not (:parent @config)))
    (watch-parent! parent))
  (swap! config merge options)
  nil)

(defn sh
  "Runs the bash script in the work folder. Its output, standard error
   included, goes to `*out*` chunk by chunk as bash wrote it, so into the
   answer, or into a string under `with-out-str`. The script gets no
   input. Answers `{:exit <code>}` once bash has ended. It does not wait
   for a process the script left in the background. An interrupt of the
   thread ends the script and everything it started."
  [script]
  (let [out *out*
        running (process/process {:err :out} "bash" "-c" (str script))
        reader (java.io.InputStreamReader. (:out running) "UTF-8")
        buffer (char-array 4096)]
    (.close (:in running))
    (try
      (loop []
        (when (.isInterrupted (Thread/currentThread))
          (throw (InterruptedException. "Interrupted.")))
        (let [alive? (.isAlive (:proc running))]
          (cond
            (.ready reader)
            (let [n (.read reader buffer)]
              (when (pos? n)
                (.write out buffer 0 n)
                (.flush out))
              (recur))

            alive?
            (do
              (Thread/sleep 20)
              (recur)))))
      {:exit (:exit @running)}
      (catch Throwable e
        (process/destroy-tree running)
        (throw e)))))

(defn- signed-url
  "A signed URL that starts where the caller of this evaluation reached
   the server, else at the server's public URL."
  [method path seconds]
  (let [{:keys [secret base-url url-seconds]} @config]
    (files/url {:rep/file-secret secret}
               {:url/method method
                :url/path path
                :url/seconds (or seconds url-seconds)
                :url/base (or *base-url* base-url)})))

(defn upload-url
  "A URL for an HTTP PUT whose body becomes the file at `path` in the work
   folder. It needs no token and expires. A file that is too large for
   one request goes up in parts: the same URL with `&offset=<n>` appends
   the body at that offset, and offset 0 starts the file over."
  ([path] (signed-url "PUT" path nil))
  ([path seconds] (signed-url "PUT" path seconds)))

(defn download-url
  "A URL that answers an HTTP GET with the file at `path` in the work
   folder. It needs no token and expires. A `Range` header fetches a
   part of the file."
  ([path] (signed-url "GET" path nil))
  ([path seconds] (signed-url "GET" path seconds)))

(defn error-data
  "The data of the exception `e` when the code itself put it there, for
   the server to show below the error."
  [e]
  (let [data (dissoc (ex-data e) :sci.impl/callstack)]
    (when-not (contains? #{:sci/error :sci.error/parse} (:type data))
      (not-empty data))))

(defn help
  "Prints what a session offers."
  []
  (println "This is babashka. Its namespaces are here: babashka.fs, babashka.process, babashka.http-client, clojure.string and the rest.")
  (if (:shared-image? @config)
    (println "All callers share this one process, the image. Definitions stay in it, and every caller sees and can redefine them. Every request starts in the namespace user.")
    (println "Definitions stay for the session. Every request starts in the namespace user."))
  (println "The value of the last form is printed as EDN, nil is not. An error ends the request at once.")
  (println)
  (println "(sh script)")
  (println "  Runs the bash script in the work folder, its output goes into the answer as bash wrote it. Answers {:exit <code>}.")
  (println "(upload-url path) (upload-url path seconds)")
  (println "  A URL for an HTTP PUT whose body becomes the file at path in the work folder.")
  (println "  A large file goes up in parts: the same URL with &offset=<bytes so far>, 0 starts the file over.")
  (println "(download-url path) (download-url path seconds)")
  (println "  A URL that answers an HTTP GET with the file at path in the work folder. A Range header fetches a part.")
  (println)
  (println "Long work: start it in a future and keep the result in an atom, then ask in a later request.")
  (println "  (def result (atom nil))")
  (println "  (def job (future (reset! result (with-out-str (sh \"./render.sh\")))))")
  (println "  later: (realized? job), @result, (future-cancel job)")
  (println "What a future prints after its request ended is lost. Keep it in the atom or in a file.")
  (if (:shared-image? @config)
    (println "A second request runs while the first is busy. It can end a hung script with pkill, or the whole image with (System/exit 0). The image then starts again by itself, without the definitions.")
    (println "A second request runs while the first is busy. It can end a hung script with pkill, or the whole session with (System/exit 0).")))

(comment
  (configure! {:secret "secret" :url-seconds 900})
  (sh "echo one; echo two >&2; exit 3")
  (with-out-str (sh "echo captured"))
  (binding [*base-url* "http://127.0.0.1:8730"]
    (upload-url "in/clip.mp4"))
  (download-url "out/final.mp4" 60))
