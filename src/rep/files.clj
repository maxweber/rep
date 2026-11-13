(ns rep.files
  "The work folder, and signed URLs that move one file into it or out of
   it. A signed URL needs no token. Its signature names the method, the
   path and the moment it expires, so it can be handed to another tool.

   The server holds a request body in memory and refuses one above
   `:rep/max-body`. So a large file comes in parts, each a PUT to the
   same signed URL with `&offset=<bytes so far>`, and goes out in parts
   with the `Range` header."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn relative
  "`path` as a clean path inside the work folder, or nil for a path that
   is absolute, leaves the folder or points into rep's own `.rep` folder."
  [path]
  (let [path (str path)]
    (when-not (or (str/blank? path)
                  (str/starts-with? path "/")
                  (str/includes? path "\\"))
      (let [clean (str (fs/normalize path))]
        (when-not (or (str/blank? clean)
                      (= ".." clean)
                      (str/starts-with? clean "../")
                      (= ".rep" clean)
                      (str/starts-with? clean ".rep/"))
          clean)))))

(defn- inside?
  "True when `target` stays in `dir` after its links are followed. A file
   that does not exist yet is judged by its nearest folder that does."
  [dir target]
  (let [existing (->> (iterate fs/parent (fs/absolutize target))
                      (take-while some?)
                      (filter fs/exists?)
                      (first))]
    (boolean (and existing
                  (fs/starts-with? (fs/real-path existing) (fs/real-path dir))))))

(defn file
  "The file of `path` in the work folder `:rep/dir`. Throws for a path
   that `relative` refuses or that a link leads out of the folder."
  [{:rep/keys [dir]} path]
  (let [clean (relative path)
        target (when clean (fs/path dir clean))]
    (when-not (and target (inside? dir target))
      (throw (ex-info (str "Not a path inside the work folder: " (pr-str path))
                      {:rep/error :path})))
    (fs/file target)))

(defn hmac
  "The HMAC-SHA256 of `text` under `secret`, as URL-safe Base64."
  [secret text]
  (let [mac (javax.crypto.Mac/getInstance "HmacSHA256")]
    (.init mac (javax.crypto.spec.SecretKeySpec. (.getBytes (str secret) "UTF-8") "HmacSHA256"))
    (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder))
                     (.doFinal mac (.getBytes (str text) "UTF-8")))))

(defn signature
  "What signs `method` on `path` until the second `expires`, under the
   key `:rep/file-secret`."
  [{:rep/keys [file-secret]} method path expires]
  (hmac file-secret (str method "\n" path "\n" expires)))

(defn- encode
  "`path` for a URL, segment by segment."
  [path]
  (->> (str/split path #"/")
       (map #(str/replace (java.net.URLEncoder/encode % "UTF-8") "+" "%20"))
       (str/join "/")))

(defn- decode
  "The path of a URL back as text. A plus sign stays a plus sign."
  [encoded]
  (->> (str/split encoded #"/")
       (map #(java.net.URLDecoder/decode (str/replace % "+" "%2B") "UTF-8"))
       (str/join "/")))

(defn now
  "The current time in seconds."
  []
  (quot (System/currentTimeMillis) 1000))

(defn url
  "A URL that allows `:url/method` (\"PUT\" or \"GET\") on `:url/path` for
   `:url/seconds` from now. `:url/base` is where the server is reached."
  [w {:url/keys [method path seconds base]}]
  (let [clean (relative path)
        expires (+ (now) seconds)]
    (when-not clean
      (throw (ex-info (str "Not a path inside the work folder: " (pr-str path))
                      {:rep/error :path})))
    (str base "/file/" (encode clean)
         "?expires=" expires
         "&signature=" (signature w method clean expires))))

(defn query
  "The parameters of a query string as a map of strings."
  [query-string]
  (into {}
        (for [pair (str/split (or query-string "") #"&")
              :let [[k v] (str/split pair #"=" 2)]
              :when (seq k)]
          [k (java.net.URLDecoder/decode (or v "") "UTF-8")])))

(defn- same?
  "Compares two strings in constant time."
  [a b]
  (java.security.MessageDigest/isEqual (.getBytes (str a) "UTF-8")
                                       (.getBytes (str b) "UTF-8")))

(defn check
  "Reads a request to `/file/<path>` and adds `:file/path` when its
   signature holds, else `:file/refused` with the reason."
  [w {:keys [request-method uri query-string]}]
  (let [method (str/upper-case (name request-method))
        path (relative (decode (subs uri (count "/file/"))))
        parameters (query query-string)
        given (get parameters "signature")
        expires (some-> (get parameters "expires") parse-long)]
    (cond
      (not path)
      (assoc w :file/refused "Not a path inside the work folder.")

      (not (and expires given (same? given (signature w method path expires))))
      (assoc w :file/refused "The signature does not fit this method and path.")

      (< expires (now))
      (assoc w :file/refused "The URL has expired.")

      :else
      (assoc w :file/path path))))

(def size-header
  "The header that tells how many bytes the file holds, on the answer
   to a PUT and on the 409 for a part at the wrong offset."
  "Rep-File-Size")

(def range-bytes
  "The most bytes one answer to a GET with a `Range` header carries,
   eight megabytes. The caller asks for the rest with its next range."
  (* 8 1024 1024))

(defn- text
  ([status body]
   (text status body nil))
  ([status body headers]
   {:status status
    :headers (merge {"Content-Type" "text/plain; charset=utf-8"} headers)
    :body body}))

(defn- size
  "The bytes of `target`, 0 without it."
  [target]
  (if (fs/regular-file? target) (fs/size target) 0))

(defonce ^{:private true
           :doc "One part is written at a time, so that the size a part is
  checked against is the size it is appended to."}
  part-lock
  (Object.))

(defn- write-part!
  "Puts `body` at `offset` of `target`. Offset 0 starts the file over,
   an offset that is the file's size appends. Answers `:part/size`, the
   bytes the file holds then. Any other offset writes nothing and
   answers `:part/conflict`, the bytes the file holds."
  [target offset body]
  (locking part-lock
    (let [held (size target)]
      (if (or (zero? offset) (= offset held))
        (do
          (io/make-parents target)
          (with-open [out (java.io.FileOutputStream. target (pos? offset))]
            (when body
              (io/copy body out)))
          {:part/size (size target)})
        {:part/conflict held}))))

(defn store!
  "Answers a signed PUT. Without `offset` in the query the body becomes
   the file. With `?offset=<n>` the body is one part of the file, for a
   file that is too large for one request: offset 0 starts the file
   over, the file's own size appends, and any other offset is a 409
   that changes nothing. Every answer but a refusal says how many
   bytes the file holds, in its text and in the header `Rep-File-Size`,
   so a caller that lost an answer knows where to go on."
  [w {:keys [body query-string] :as request}]
  (let [{:file/keys [path refused] :as w} (check w request)
        given (get (query query-string) "offset")
        offset (if given (parse-long given) 0)]
    (cond
      refused
      (text 403 (str refused "\n"))

      (not (and offset (not (neg? offset))))
      (text 400 "The offset has to be a number of bytes, 0 or more.\n")

      :else
      (let [target (file w path)
            {:part/keys [size conflict]} (write-part! target offset body)]
        (if conflict
          (text 409
                (str path " holds " conflict " bytes, so its next part starts at offset " conflict
                     ", not at " offset ".\n")
                {size-header (str conflict)})
          (text 201
                (str "Stored " path " (" size " bytes).\n")
                {size-header (str size)}))))))

(defn byte-range
  "The inclusive positions `[start end]` that the `Range` header asks
   for in a file of `length` bytes, at most `range-bytes` of them. Nil
   without the header or for one that is not `bytes=<start>-` or
   `bytes=<start>-<end>`, and `:beyond` when it starts past the end."
  [header length]
  (when-let [[_ from to] (re-matches #"bytes=(\d+)-(\d*)" (str/trim (str header)))]
    (let [start (parse-long from)
          end (min (if (seq to) (parse-long to) Long/MAX_VALUE)
                   (dec length)
                   (+ start (dec range-bytes)))]
      (cond
        (>= start length) :beyond
        (<= start end) [start end]))))

(defn- read-bytes
  "The bytes `start` to `end` of `target`, both included."
  [target start end]
  (with-open [in (java.io.RandomAccessFile. (str target) "r")]
    (let [buffer (byte-array (inc (- end start)))]
      (.seek in start)
      (.readFully in buffer)
      buffer)))

(defn fetch
  "Answers a signed GET with the file. With a `Range` header it answers
   206 and that part of the file, at most `range-bytes` of it. The
   header `Content-Range` says which bytes came and how many the file
   has. A range that starts past the end is a 416."
  [w request]
  (let [{:file/keys [path refused] :as w} (check w request)
        target (when path (file w path))
        headers {"Content-Type" "application/octet-stream"
                 "Accept-Ranges" "bytes"}]
    (cond
      refused
      (text 403 (str refused "\n"))

      (not (fs/regular-file? target))
      (text 404 (str "No file " path ".\n"))

      :else
      (let [length (fs/size target)
            wanted (byte-range (get-in request [:headers "range"]) length)]
        (cond
          (= :beyond wanted)
          (text 416
                (str path " holds " length " bytes.\n")
                {"Content-Range" (str "bytes */" length)})

          wanted
          (let [[start end] wanted]
            {:status 206
             :headers (assoc headers "Content-Range" (str "bytes " start "-" end "/" length))
             :body (java.io.ByteArrayInputStream. (read-bytes target start end))})

          :else
          {:status 200
           :headers headers
           :body target})))))

(comment
  (def w {:rep/dir "work" :rep/file-secret "secret"})
  (relative "a/../b.txt")
  (relative "../b.txt")
  (url w {:url/method "PUT" :url/path "in/clip one.mp4" :url/seconds 900
          :url/base "http://127.0.0.1:8730"})
  (byte-range "bytes=0-99" 1000)
  (byte-range "bytes=900-" 1000)
  (byte-range "bytes=1000-" 1000))
