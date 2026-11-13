(ns rep.files
  "The work folder, and signed URLs that move one file into it or out of
   it. A signed URL needs no token. Its signature names the method, the
   path and the moment it expires, so it can be handed to another tool.

   The server holds a request body in memory and refuses one above
   `:rep/max-body`. So a large file comes in parts, each a PUT to the
   same signed URL with `&offset=<bytes so far>`, and goes out in parts
   with the `Range` header. The sender of the parts names its upload
   with `&upload=<id>`, so that a second sender of the same path is
   told and does not mix its parts in."
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

(defn encode
  "`path` for a URL, segment by segment."
  [path]
  (->> (str/split path #"/")
       (map #(str/replace (java.net.URLEncoder/encode % "UTF-8") "+" "%20"))
       (str/join "/")))

(defn decode
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

(def max-file-mb
  "How large a file may grow through its parts when the system names no
   `:rep/max-file-mb`: 4096 megabytes. A signed URL for a PUT is good
   for many parts, so without a limit it could fill the disk."
  4096)

(defonce ^{:private true
           :doc "One part is written at a time, so that the size a part is
  checked against is the size it is appended to."}
  part-lock
  (Object.))

(defn- upload-record
  "The file that names the upload that writes `path` right now, in the
   state folder beside the work folder's files. Nil for a system
   without a state folder."
  [{:rep/keys [state-dir]} path]
  (when state-dir
    (fs/path state-dir "uploads" (hmac "rep upload" path))))

(defn- writer
  "The id of the upload that writes `path`, nil when none is on record."
  [w path]
  (let [record (upload-record w path)]
    (when (and record (fs/regular-file? record))
      (not-empty (str/trim (slurp (fs/file record)))))))

(defn- note-writer!
  "Keeps on record which upload writes `path` after its part at
   `offset` went in. The part at offset 0 starts the file over, so its
   upload is the writer from there on, and a part without an id clears
   the record. A later part puts its id on record when none is there,
   or when it took the file over."
  [w path {:part/keys [offset upload]}]
  (when-let [record (upload-record w path)]
    (cond
      upload (when (not= upload (writer w path))
               (fs/create-dirs (fs/parent record))
               (spit (fs/file record) upload))
      (zero? offset) (fs/delete-if-exists record)))
  nil)

(defn- write-part!
  "Puts `:part/body` at `:part/offset` of `target`, the file of `path`.
   Offset 0 starts the file over, an offset that is the file's size
   appends. Answers `:part/size`, the bytes the file holds then.
   Otherwise it writes nothing and answers the reason:

     :part/taken      another upload writes this file: the part is not
                      the first one and names another `:part/upload`
                      than the one on record, without `:part/resume?`
     :part/too-large  the file would grow past this many bytes
     :part/conflict   the part does not start where the file ends, the
                      value is the bytes the file holds"
  [w target path {:part/keys [offset body length upload resume?] :as part}]
  (locking part-lock
    (let [held (size target)
          recorded (writer w path)
          limit (* 1024 1024 (or (:rep/max-file-mb w) max-file-mb))]
      (cond
        (and (pos? offset) upload recorded (not= upload recorded) (not resume?))
        {:part/taken recorded}

        (< limit (+ offset length))
        {:part/too-large limit}

        (not (or (zero? offset) (= offset held)))
        {:part/conflict held}

        :else
        (do
          (io/make-parents target)
          (with-open [out (java.io.FileOutputStream. target (pos? offset))]
            (when body
              (io/copy body out)))
          (note-writer! w path part)
          {:part/size (size target)})))))

(defn- body-length
  "How many bytes the body of `request` has."
  [{:keys [body headers]}]
  (or (some-> (get headers "content-length") parse-long)
      (when (instance? java.io.InputStream body)
        (.available body))
      0))

(defn store!
  "Answers a signed PUT. Without `offset` in the query the body becomes
   the file. With `?offset=<n>` the body is one part of the file, for a
   file that is too large for one request: offset 0 starts the file
   over, the file's own size appends, and any other offset is a 409
   that changes nothing. Every answer to a part that was stored or that
   came at the wrong offset says how many bytes the file holds, in its
   text and in the header `Rep-File-Size`, so a caller that lost an
   answer knows where to go on.

   A file grows to `:rep/max-file-mb` megabytes at most. A part that
   would pass that is a 413.

   `upload=<id>` names the upload a part belongs to, an id its sender
   made up. The part at offset 0 puts the id on record. A later part
   with another id is a 409 without `Rep-File-Size` that says another
   upload writes the file, so two senders of one path never mix their
   parts unnoticed. With `resume=true` a later part takes the file over
   for its id, which is how a sender goes on after it lost its own. A
   part without an id is taken as before."
  [w {:keys [body query-string] :as request}]
  (let [{:file/keys [path refused] :as w} (check w request)
        {:strs [upload resume] given "offset"} (query query-string)
        offset (if given (parse-long given) 0)]
    (cond
      refused
      (text 403 (str refused "\n"))

      (not (and offset (not (neg? offset))))
      (text 400 "The offset has to be a number of bytes, 0 or more.\n")

      :else
      (let [target (file w path)
            {:part/keys [size conflict taken too-large]}
            (write-part! w target path {:part/offset offset
                                        :part/body body
                                        :part/length (body-length request)
                                        :part/upload (not-empty upload)
                                        :part/resume? (= "true" resume)})]
        (cond
          taken
          (text 409 (str "Another upload writes " path ". This part belongs to an upload that does not."
                         " Start over at offset 0, or take the file over with resume=true.\n"))

          too-large
          (text 413 (str path " would grow past " (quot too-large (* 1024 1024))
                         " MB, the largest file this server takes in parts (:rep/max-file-mb).\n"))

          conflict
          (text 409
                (str path " holds " conflict " bytes, so its next part starts at offset " conflict
                     ", not at " offset ".\n")
                {size-header (str conflict)})

          :else
          (text 201
                (str "Stored " path " (" size " bytes).\n")
                {size-header (str size)}))))))

(defn forget-uploads!
  "Deletes the records of uploads that began more than `:rep/keep-days`
   ago (7 without the option, never with 0). The clock is `:rep/now-ms`
   in epoch milliseconds, the system's clock without it. An upload that
   still runs then puts its id on record again with its next part.
   Answers how many records it deleted."
  [{:rep/keys [state-dir keep-days now-ms]}]
  (let [days (or keep-days 7)
        before (- (or now-ms (System/currentTimeMillis)) (* days 24 60 60 1000))
        folder (when state-dir (fs/path state-dir "uploads"))
        old (when (and folder (pos? days) (fs/directory? folder))
              (filterv #(< (fs/file-time->millis (fs/last-modified-time %)) before)
                       (fs/list-dir folder)))]
    (run! fs/delete-if-exists old)
    (count old)))

(defn byte-range
  "What the `Range` header asks for in a file of `length` bytes: nil
   without a header, the inclusive positions `[start end]` for
   `bytes=<start>-` and `bytes=<start>-<end>`, at most `range-bytes` of
   them, and `:unsatisfiable` for a header of any other shape, for a
   start past the end of the file and for a start after its end."
  [header length]
  (when (some? header)
    (let [[_ from to] (re-matches #"bytes=(\d+)-(\d*)" (str/trim (str header)))
          start (some-> from parse-long)
          end (when start
                (min (or (when (seq to) (parse-long to)) Long/MAX_VALUE)
                     (dec length)
                     (+ start (dec range-bytes))))]
      (if (and start (< start length) (<= start end))
        [start end]
        :unsatisfiable))))

(defn- read-bytes
  "The bytes `start` to `end` of `target`, both included."
  [target start end]
  (with-open [in (java.io.RandomAccessFile. (str target) "r")]
    (let [buffer (byte-array (inc (- end start)))]
      (.seek in start)
      (.readFully in buffer)
      buffer)))

(defn serve
  "Answers `request` with the file at `path` of the work folder
   `:rep/dir`, for a caller that has checked who asks: `fetch` after the
   signature, a host after a credential of its own. The path gets the
   checks of `file`. With a `Range` header it answers 206 and that part
   of the file, at most `range-bytes` of it, and `Content-Range` says
   which bytes came and how many the file has. Else it answers

     403  the path is no path inside the work folder
     404  there is no such file
     416  the range is malformed or names no byte of the file"
  [w request path]
  (let [target (try
                 (file w path)
                 (catch clojure.lang.ExceptionInfo _ nil))
        headers {"Content-Type" "application/octet-stream"
                 "Accept-Ranges" "bytes"}]
    (cond
      (nil? target)
      (text 403 "Not a path inside the work folder.\n")

      (not (fs/regular-file? target))
      (text 404 (str "No file " path ".\n"))

      :else
      (let [length (fs/size target)
            wanted (byte-range (get-in request [:headers "range"]) length)]
        (cond
          (= :unsatisfiable wanted)
          (text 416
                (str path " holds " length " bytes. Ask for a range as bytes=<start>-<end> or bytes=<start>-.\n")
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

(defn fetch
  "Answers a signed GET with the file, see `serve`: the whole file, or
   with a `Range` header a part of it."
  [w request]
  (let [{:file/keys [path refused] :as w} (check w request)]
    (if refused
      (text 403 (str refused "\n"))
      (serve w request path))))

(comment
  (def w {:rep/dir "work" :rep/file-secret "secret"})
  (relative "a/../b.txt")
  (relative "../b.txt")
  (url w {:url/method "PUT" :url/path "in/clip one.mp4" :url/seconds 900
          :url/base "http://127.0.0.1:8730"})
  (byte-range "bytes=0-99" 1000)
  (byte-range "bytes=900-" 1000)
  (byte-range "bytes=1000-" 1000)
  (byte-range "bytes=99999999999999999999-" 1000)
  (serve w {:headers {"range" "bytes=0-9"}} "in/clip one.mp4")
  (forget-uploads! {:rep/state-dir "work/.rep"}))
