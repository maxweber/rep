(ns rep.door
  "The listener in front of http-kit, for whoever turns it on with
   `:rep/door?`. It is there for one reason: a file of any size comes
   down in one plain GET, with its length in `Content-Length`, while
   the server's memory stays flat.

   It is off by default. The door is an HTTP listener of our own in
   front of every request on the public port, and that job usually
   belongs to a proven server. Without the door a plain GET sends a
   file up to 2 GB, and a larger one comes in parts with `Range`, which
   `rep download` does.

   http-kit cannot send a larger file in one answer, which the source
   says and a probe confirmed (babashka 1.13, http-kit 2.9):

   - A response that does not stream takes its `Content-Length` from
     one `ByteBuffer`, and a `ByteBuffer` ends at 2 GB. A `File` above
     that is refused with a 500, \"File is too large to buffer\". Below
     it http-kit maps the whole file into memory.
   - A response that streams over a channel is sent chunked to an
     HTTP/1.1 client, and http-kit takes a `Content-Length` header off
     it. A channel also has no way to wait for the client: what is sent
     faster than the client reads is queued.

   So with the door on, the door listens on the public address, and
   http-kit listens on a port of 127.0.0.1 behind it. The door reads
   the head of every request of a connection and does one of two
   things with it.

   A GET or a HEAD of a path that `:door/own?` claims, the file routes,
   it answers itself. It calls the same ring handler http-kit would
   call, so the checks of who may read which file stay in one place,
   and writes what the handler answers. The request map it hands over
   has `:door/request?` true. A body that is a `File` goes
   out slice by slice with a write that blocks until the client has
   taken it. That is the writer that waits for the client: the door
   holds one slice of the file at a time, however large the file and
   however slow the client, and when the client hangs up the write
   throws, the file closes and the thread ends.

   Every other request it passes on to http-kit as it is, body
   included, and what http-kit answers flows back as it comes, a frame
   stream that never ends as well. The door never holds a body.

   The door answers a file request when it has read it. HTTP allows a
   client to send a request before it has the answer to the one before,
   which no browser and none of our clients does. Such a client could
   get the answer to a file request before the answer that http-kit
   still owes it.

   A connection costs a thread, two once it has talked to http-kit.
   `max-connections` bounds them. http-kit sees every connection come
   from 127.0.0.1, so a handler that wants the client's address reads
   it from the request the door hands it, or from a header a proxy
   set.

   Known and not fixed (review of 2026-11-13, see the README): the
   accept loop ends for good when the process runs out of file
   descriptors, a connection has no time limit, and a protocol upgrade
   cannot pass. Fix the first two before the door faces the internet."
  (:require [clojure.string :as str]))

(def slice-bytes
  "How much of a file the door holds at a time while it sends it, when
   nobody names `:door/slice-bytes`: one megabyte."
  (* 1024 1024))

(def head-bytes
  "The longest head of a request the door reads. A longer one is a 431."
  65536)

(def max-connections
  "How many connections the door serves at once. One more is closed at
   once, so that whoever opens connections and says nothing cannot take
   every thread."
  1024)

(def reasons
  "The words after the status in the first line of an answer."
  {200 "OK" 201 "Created" 206 "Partial Content" 304 "Not Modified"
   400 "Bad Request" 401 "Unauthorized" 403 "Forbidden" 404 "Not Found"
   405 "Method Not Allowed" 409 "Conflict" 413 "Content Too Large"
   416 "Range Not Satisfiable" 431 "Request Header Fields Too Large"
   500 "Internal Server Error" 502 "Bad Gateway" 503 "Service Unavailable"})

(defn- note
  "A line on standard error."
  [& words]
  (binding [*out* *err*]
    (println (str/join " " (cons "rep.door:" words)))))

(defn- read-head
  "Reads from the pushback stream `in` up to the empty line that ends
   the head of a request. Answers the head as text, nil when the stream
   ended first, and `:too-long` for a head above `head-bytes`. What came
   after the head goes back into the stream."
  [in]
  (let [buffer (byte-array 8192)
        seen (java.io.ByteArrayOutputStream.)]
    (loop []
      (let [n (.read in buffer)]
        (when (pos? n)
          (.write seen buffer 0 n)
          (let [text (.toString seen "ISO-8859-1")
                end (.indexOf text "\r\n\r\n")]
            (cond
              (<= 0 end)
              (let [used (+ end 4)
                    all (.toByteArray seen)]
                (when (< used (alength all))
                  (.unread in all used (- (alength all) used)))
                (subs text 0 used))

              (< head-bytes (.size seen))
              :too-long

              :else
              (recur))))))))

(defn parse-head
  "The head of a request as a map: `:door/method`, `:door/version`,
   `:door/head`, the text itself, and what a ring handler reads, `:uri`,
   `:query-string` and `:headers` with lower-case names. Nil for a text
   that is no head of an HTTP/1 request."
  [text]
  (let [[line & more] (str/split-lines (str/triml (str text)))
        [_ method target version] (re-matches #"([A-Z]+) (\S+) (HTTP/1\.[01])" (str line))
        [uri query] (str/split (str target) #"\?" 2)]
    (when method
      {:door/method method
       :door/version version
       :door/head (str text)
       :uri uri
       :query-string query
       :headers (reduce (fn [headers line]
                          (if-let [[_ k v] (re-matches #"([^:\s]+):[ \t]*(.*?)[ \t]*" line)]
                            (update headers (str/lower-case k) #(if % (str % "," v) v))
                            headers))
                        {}
                        more)})))

(defn- token?
  "True when the header `k` of `request` lists `word`, case ignored."
  [request k word]
  (boolean (some #(= word (str/lower-case (str/trim %)))
                 (str/split (get-in request [:headers k] "") #","))))

(defn- copy!
  "Copies exactly `n` bytes from `in` to `out`, `buffer` at a time.
   Throws when `in` ends first."
  [in out buffer n]
  (loop [left n]
    (when (pos? left)
      (let [read (.read in buffer 0 (int (min left (alength buffer))))]
        (when (neg? read)
          (throw (java.io.EOFException. "The stream ended in the middle of a body.")))
        (when out
          (.write out buffer 0 read))
        (recur (- left read))))))

(defn- read-line!
  "Reads one line from `in`, its line break included, as bytes."
  [in]
  (let [line (java.io.ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (when (neg? b)
          (throw (java.io.EOFException. "The stream ended in the middle of a line.")))
        (.write line b)
        (if (= 10 b)
          (.toByteArray line)
          (recur))))))

(defn- copy-chunked!
  "Copies a chunked body from `in` to `out` as it is: every chunk with
   its size line, the last one of size 0, and the trailer up to its
   empty line."
  [in out buffer]
  (loop []
    (let [line (read-line! in)
          size (Long/parseLong (str/trim (first (str/split (String. line "ISO-8859-1") #";"))) 16)]
      (when out
        (.write out line))
      (if (pos? size)
        (do
          (copy! in out buffer (+ size 2))
          (recur))
        (loop []
          (let [trailer (read-line! in)]
            (when out
              (.write out trailer))
            (when (< 2 (alength trailer))
              (recur))))))))

(defn- copy-body!
  "Copies the body of `request` from `in` to `out`, or reads it away
   when `out` is nil. A request without `Content-Length` and without a
   chunked `Transfer-Encoding` has none."
  [request in out buffer]
  (cond
    (token? request "transfer-encoding" "chunked")
    (copy-chunked! in out buffer)

    (get-in request [:headers "content-length"])
    (copy! in out buffer (or (parse-long (str/trim (get-in request [:headers "content-length"]))) 0))))

(defn- body-of
  "The body of a ring response as `[length write!]`: how many bytes it
   has, and a function that writes them to a stream. A `File` is
   written `slice` bytes at a time, with a write that waits for the
   client. Every other body is small by its nature, an error's text or
   the bytes of one range."
  [body slice]
  (cond
    (nil? body)
    [0 (fn [_out] nil)]

    (instance? java.io.File body)
    (let [length (.length body)]
      [length (fn [out]
                (with-open [in (java.io.FileInputStream. body)]
                  (copy! in out (byte-array (int (max 1 (min slice length)))) length)))])

    (instance? java.io.InputStream body)
    (let [bytes (.readAllBytes body)]
      [(alength bytes) (fn [out] (.write out bytes))])

    (bytes? body)
    [(alength body) (fn [out] (.write out body))]

    :else
    (let [bytes (.getBytes (str body) "UTF-8")]
      [(alength bytes) (fn [out] (.write out bytes))])))

(defn- write-response!
  "Writes the ring response `response` to `out` as the answer to
   `request`: the status line, the headers with the `Content-Length`
   of the body, and the body, none for a HEAD. Answers true when the
   connection stays open for the next request."
  [out request {:keys [status headers body]} slice]
  (let [[length write!] (body-of body slice)
        status (or status 200)
        keep? (and (= "HTTP/1.1" (:door/version request))
                   (not (token? request "connection" "close")))
        head (str/join
              "\r\n"
              (concat [(str "HTTP/1.1 " status " " (get reasons status "Status"))]
                      (for [[k v] headers
                            :when (not (contains? #{"content-length" "connection" "transfer-encoding"}
                                                  (str/lower-case (name k))))]
                        (str (name k) ": " v))
                      [(str "Content-Length: " length)
                       (str "Connection: " (if keep? "keep-alive" "close"))
                       ""
                       ""]))]
    (.write out (.getBytes head "ISO-8859-1"))
    (when-not (= "HEAD" (:door/method request))
      (write! out))
    (.flush out)
    keep?))

(defn- plain
  "A ring response with `text` as its body."
  [status text]
  {:status status
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body text})

(defn- answer
  "What the ring handler of the door answers to `request`, a 500 when
   it throws."
  [{:door/keys [handler port]} request socket]
  (try
    (handler {:request-method (keyword (str/lower-case (:door/method request)))
              :uri (:uri request)
              :query-string (:query-string request)
              :headers (:headers request)
              :scheme :http
              :server-port port
              :remote-addr (some-> (.getInetAddress socket) (.getHostAddress))
              :body nil
              :door/request? true})
    (catch Throwable e
      (note "the handler threw for" (:uri request) "-" (or (ex-message e) (.getName (class e))))
      (plain 500 "The server could not answer this request.\n"))))

(defn- own?
  "True for a request the door answers itself: a GET or a HEAD of a
   path that `:door/own?` claims."
  [door request]
  (boolean (and (contains? #{"GET" "HEAD"} (:door/method request))
                ((:door/own? door) (:uri request)))))

(defn- close!
  [closeable]
  (try
    (some-> closeable (.close))
    (catch java.io.IOException _ nil)))

(defn- pump!
  "Copies what http-kit answers on `inner` to the client as it comes,
   until one of the two is gone, and closes the client then: an answer
   that http-kit ends by closing ends for the client the same way."
  [inner client out]
  (try
    (let [in (.getInputStream inner)
          buffer (byte-array 65536)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (locking out
              (.write out buffer 0 n)
              (.flush out))
            (recur)))))
    (catch java.io.IOException _ nil)
    (finally
      (close! client))))

(defn- connect!
  "Opens the connection to http-kit for one client and starts the
   thread that carries the answers back. Answers the socket."
  [{:door/keys [inner-port]} client out]
  (let [inner (java.net.Socket. "127.0.0.1" (int inner-port))]
    (.setTcpNoDelay inner true)
    (doto (Thread. #(pump! inner client out))
      (.setDaemon true)
      (.start))
    inner))

(defn- pass-on!
  "Passes the request `request` on to http-kit over `inner`: its head
   as it came, then its body from `in`."
  [request in inner buffer]
  (let [to (.getOutputStream inner)]
    (.write to (.getBytes (:door/head request) "ISO-8859-1"))
    (copy-body! request in to buffer)
    (.flush to)))

(defn- converse!
  "Serves one connection until the client or http-kit ends it: reads a
   request's head, answers it or passes it on, and reads the next."
  [{:door/keys [slice-bytes] :as door} client]
  (let [in (java.io.PushbackInputStream. (.getInputStream client) 8192)
        out (.getOutputStream client)
        buffer (byte-array 65536)
        inner (atom nil)]
    (try
      (loop []
        (let [head (read-head in)
              request (when (string? head) (parse-head head))]
          (cond
            (nil? head)
            nil

            (nil? request)
            (locking out
              (write-response! out {:door/version "HTTP/1.0"}
                               (if (= :too-long head)
                                 (plain 431 "The head of the request is too long.\n")
                                 (plain 400 "This is no HTTP/1 request.\n"))
                               slice-bytes))

            (own? door request)
            (do
              (copy-body! request in nil buffer)
              (when (locking out
                      (write-response! out request (answer door request client) slice-bytes))
                (recur)))

            :else
            (do
              (when-not @inner
                (reset! inner (connect! door client out)))
              (pass-on! request in @inner buffer)
              (recur)))))
      (catch java.io.IOException _ nil)
      (catch Throwable e
        (note "a connection broke:" (or (ex-message e) (.getName (class e)))))
      (finally
        (close! @inner)
        (close! client)))))

(defn- accept!
  "The life of the door's listener: takes every connection and gives it
   a thread, until the listener closes."
  [{:door/keys [listener connections] :as door}]
  (loop []
    (when-let [client (try
                        (.accept listener)
                        (catch java.io.IOException _ nil))]
      (if (< (count @connections) max-connections)
        (do
          (.setTcpNoDelay client true)
          (swap! connections conj client)
          (doto (Thread. (fn []
                           (try
                             (converse! door client)
                             (finally
                               (swap! connections disj client)))))
            (.setDaemon true)
            (.start)))
        (close! client))
      (recur))))

(defn start!
  "Starts the door. It listens on `:door/ip` and `:door/port`, 0 for any
   free port, and has http-kit behind it on `:door/inner-port` of
   127.0.0.1. `:door/handler` is the ring handler that http-kit calls
   too, and `:door/own?` says for a request's path whether the door
   answers a GET of it itself. `:door/slice-bytes` is how much of a file
   it holds at a time. Answers the door with `:door/port`, the port it
   listens on, and `:door/connections`, an atom with the connections
   that are open."
  [{:door/keys [ip port] :as options}]
  (let [listener (java.net.ServerSocket. (int (or port 0)) 1024 (java.net.InetAddress/getByName (or ip "127.0.0.1")))
        door (merge {:door/slice-bytes slice-bytes}
                    options
                    {:door/listener listener
                     :door/port (.getLocalPort listener)
                     :door/connections (atom #{})})]
    (doto (Thread. #(accept! door))
      (.setDaemon true)
      (.start))
    door))

(defn stop!
  "Closes the door's listener and every connection that is open, which
   ends their threads. Answers nil."
  [{:door/keys [listener connections]}]
  (close! listener)
  (run! close! @connections)
  nil)

(comment
  (require '[org.httpkit.server :as http])
  (defn handler
    [request]
    (if (str/starts-with? (:uri request) "/file/")
      {:status 200 :headers {"Content-Type" "application/octet-stream"} :body (java.io.File. "README.md")}
      {:status 200 :body "from http-kit"}))
  (def inner (http/run-server handler {:ip "127.0.0.1" :port 0 :legacy-return-value? false}))
  (def door (start! {:door/port 8732
                     :door/inner-port (http/server-port inner)
                     :door/handler handler
                     :door/own? #(str/starts-with? % "/file/")}))
  ;; curl -sI http://127.0.0.1:8732/file/x    answered by the door
  ;; curl -s  http://127.0.0.1:8732/other     passed on to http-kit
  (parse-head "GET /file/a%20b?expires=1 HTTP/1.1\r\nHost: x\r\nRange: bytes=0-9\r\n\r\n")
  (count @(:door/connections door))
  (stop! door)
  (http/server-stop! inner))
