(ns rep.files
  "The work folder, and the two requests that move one file into it or
   out of it: `PUT /file/<path>` and `GET /file/<path>`. Who may send
   them is not decided here. The server checks the bearer token first
   (`rep.server/handle`), a host that serves files under a credential
   of its own checks that, and both then call `store!` and `serve`.

   The server holds a request body in memory and refuses one above
   `:rep/max-body`. So a large file comes in parts, each a PUT to the
   same path with `?offset=<bytes so far>`, and goes out in parts with
   the `Range` header. The sender of the parts names its upload with
   `&upload=<id>`, so that a second sender of the same path is told
   and does not mix its parts in."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn relative
  "`path` as a clean path inside the work folder, or nil for a path that
   is absolute, leaves the folder or points into rep's own `.rep` folder.
   This looks at the name only. `file` looks at where the path leads."
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

(defn- state-folder
  "The folder rep keeps its own files in, the code and the output of
   every caller's evaluations: `:rep/state-dir`, or `.rep` in the work
   folder without it."
  [{:rep/keys [dir state-dir]}]
  (or state-dir (fs/path dir ".rep")))

(defn- within?
  "True when the real path `real` lies in the folder `dir`, links in
   `dir` followed. False for a folder that is not there."
  [dir real]
  (and (fs/exists? dir)
       (fs/starts-with? real (fs/real-path dir))))

(defn- allowed?
  "True when `target` resolves to a place inside the work folder and
   outside rep's state folder. What counts is the real path, with every
   link followed: a link in the work folder that leads out of it, or
   into the state folder, makes its path as forbidden as the place it
   leads to. A file that does not exist yet is judged by its nearest
   folder that does. A link that leads nowhere is refused, a write
   through it would make a file wherever it points."
  [{:rep/keys [dir] :as w} target]
  (let [existing (->> (iterate fs/parent (fs/absolutize target))
                      (take-while some?)
                      (filter #(fs/exists? % {:nofollow-links true}))
                      (first))]
    (boolean
     (when (and existing (fs/exists? existing))
       (let [real (fs/real-path existing)]
         (and (within? dir real)
              (not (within? (state-folder w) real))))))))

(defn file
  "The file of `path` in the work folder `:rep/dir`. Throws for a path
   that `relative` refuses, and for one that resolves, through a link,
   to a place outside the work folder or inside rep's state folder.
   `store!` and `serve` come through here, so rep's own routes and a
   host's."
  [{:rep/keys [dir] :as w} path]
  (let [clean (relative path)
        target (when clean (fs/path dir clean))]
    (when-not (and target (allowed? w target))
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

(defn decode
  "The path of a URL back as text. A plus sign stays a plus sign."
  [encoded]
  (->> (str/split encoded #"/")
       (map #(java.net.URLDecoder/decode (str/replace % "+" "%2B") "UTF-8"))
       (str/join "/")))

(defn query
  "The parameters of a query string as a map of strings."
  [query-string]
  (into {}
        (for [pair (str/split (or query-string "") #"&")
              :let [[k v] (str/split pair #"=" 2)]
              :when (seq k)]
          [k (java.net.URLDecoder/decode (or v "") "UTF-8")])))

(defn requested
  "The path that a request to `/file/<path>` names, as `relative`
   cleans it, or nil for a path that it refuses. This looks at the name
   only: `store!` and `serve` look at where the path leads."
  [{:keys [uri]}]
  (try
    (relative (decode (subs (str uri) (count "/file/"))))
    (catch IllegalArgumentException _ nil)))

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
   `:rep/max-file-mb`: 4096 megabytes. A path takes one part after the
   other, so without a limit a file could fill the disk."
  4096)

(defonce ^{:private true
           :doc "One part is written at a time, so that the size a part is
  checked against is the size it is appended to."}
  part-lock
  (Object.))

(def upload-idle-ms
  "How long an upload may send no part before it counts as over: 15
   minutes. Its record is no record from then on, so that an upload
   that ended, or was given up, does not keep the file to itself."
  (* 15 60 1000))

(defn- upload-record
  "The file that names the upload that writes `path` right now, in the
   state folder beside the work folder's files. Nil for a system
   without a state folder."
  [{:rep/keys [state-dir]} path]
  (when state-dir
    (fs/path state-dir "uploads" (hmac "rep upload" path))))

(defn- live?
  "True when the upload record `record` got a part within
   `upload-idle-ms`. The clock is `:rep/now-ms`, the system's without
   it."
  [{:rep/keys [now-ms]} record]
  (and (fs/regular-file? record)
       (< (- (or now-ms (System/currentTimeMillis))
             (fs/file-time->millis (fs/last-modified-time record)))
          upload-idle-ms)))

(defn- writer
  "The id of the upload that writes `path` right now, nil when none is
   on record or the one on record sent no part for `upload-idle-ms`."
  [w path]
  (let [record (upload-record w path)]
    (when (and record (live? w record))
      (not-empty (str/trim (slurp (fs/file record)))))))

(defn- note-writer!
  "Puts the upload `:part/upload` on record as the one that writes
   `path`, with now as the time of its last part. A part without an id
   leaves no record."
  [{:rep/keys [now-ms] :as w} path {:part/keys [upload]}]
  (when-let [record (and upload (upload-record w path))]
    (fs/create-dirs (fs/parent record))
    (spit (fs/file record) upload)
    (when now-ms
      (fs/set-last-modified-time record (java.nio.file.attribute.FileTime/fromMillis now-ms))))
  nil)

(defn- refusal
  "Why the part may not go into the file that the upload `recorded`
   writes, as a keyword, or nil when it may. A part of an upload has an
   id: it is taken when it starts the file over at offset 0, when it
   takes the file over with `:part/resume?`, or when its id is the one
   on record. So a later part needs a record with its own id, and a
   missing record refuses it like another upload's. A part without an
   id is taken only while no upload is on record."
  [recorded {:part/keys [offset upload resume?]}]
  (cond
    (and upload (or (zero? offset) resume? (= upload recorded))) nil
    (and upload recorded) :another-upload
    upload :no-upload
    recorded :no-id))

(defn- write-part!
  "Puts `:part/body` at `:part/offset` of `target`, the file of `path`.
   Offset 0 starts the file over, an offset that is the file's size
   appends. Answers `:part/size`, the bytes the file holds then.
   Otherwise it writes nothing and answers the reason:

     :part/refused    the part does not belong to the upload that writes
                      this file, see `refusal`
     :part/too-large  the file would grow past this many bytes
     :part/conflict   the part does not start where the file ends, the
                      value is the bytes the file holds"
  [w target path {:part/keys [offset body length] :as part}]
  (locking part-lock
    (let [held (size target)
          refused (refusal (writer w path) part)
          limit (* 1024 1024 (or (:rep/max-file-mb w) max-file-mb))]
      (cond
        refused
        {:part/refused refused}

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

(def refusals
  "The sentence for each reason of `refusal`. `%s` is the path."
  {:another-upload (str "Another upload writes %s. This part belongs to an upload that does not."
                        " Start over at offset 0, or take the file over with resume=true.")
   :no-upload (str "No upload with this id writes %s. An upload that sent no part for 15 minutes is over."
                   " Start over at offset 0, or go on with resume=true.")
   :no-id (str "An upload writes %s, and this part names none."
               " Send upload=<an id of your own> with offset=0 to start the file over.")})

(defn- target
  "The file of `path` in the work folder, or nil when `file` refuses
   the path."
  [w path]
  (try
    (file w path)
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn- body-length
  "How many bytes the body of `request` has."
  [{:keys [body headers]}]
  (or (some-> (get headers "content-length") parse-long)
      (when (instance? java.io.InputStream body)
        (.available body))
      0))

(defn store!
  "Answers a PUT of the file at `path` of the work folder, for a caller
   that has checked who asks: the server after the bearer token, a host
   after a credential of its own. The path gets the checks of `file`.

   Without `offset` in the query the body becomes the file, and a path
   that `file` refuses is a 403. With `?offset=<n>` the body is one
   part of the file, for a file that is too large for one request:
   offset 0 starts the file over, the file's own size appends, and any
   other offset is a 409 that changes nothing. Every answer to a part
   that was stored or that came at the wrong offset says how many bytes
   the file holds, in its text and in the header `Rep-File-Size`, so a
   caller that lost an answer knows where to go on.

   A file grows to `:rep/max-file-mb` megabytes at most. A part that
   would pass that is a 413.

   `upload=<id>` names the upload a part belongs to, an id its sender
   made up. The part at offset 0 puts the id on record, and every part
   after it needs a record with its own id. A part with another id, a
   part whose upload is not on record, and a part without an id while
   an upload is on record are each a 409 without `Rep-File-Size` that
   says why, so two senders of one path never mix their parts
   unnoticed. With `resume=true` a later part takes the file over for
   its id, which is how a sender goes on after it lost its own. An
   upload that sent no part for 15 minutes is over and off the record.
   While no upload is on record, a part without an id is taken as
   before."
  [w {:keys [body query-string] :as request} path]
  (let [{:strs [upload resume] given "offset"} (query query-string)
        offset (if given (parse-long given) 0)]
    (cond
      (not (and offset (not (neg? offset))))
      (text 400 "The offset has to be a number of bytes, 0 or more.\n")

      (nil? (target w path))
      (text 403 "Not a path inside the work folder.\n")

      :else
      (let [target (target w path)
            {:part/keys [size conflict refused too-large]}
            (write-part! w target path {:part/offset offset
                                        :part/body body
                                        :part/length (body-length request)
                                        :part/upload (not-empty upload)
                                        :part/resume? (= "true" resume)})]
        (cond
          refused
          (text 409 (str (format (get refusals refused) path) "\n"))

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
  "Deletes the records of uploads that are over: those that got no part
   for `upload-idle-ms`. The clock is `:rep/now-ms` in epoch
   milliseconds, the system's clock without it. Answers how many
   records it deleted."
  [{:rep/keys [state-dir] :as w}]
  (let [folder (when state-dir (fs/path state-dir "uploads"))
        over (when (and folder (fs/directory? folder))
               (filterv #(not (live? w %)) (fs/list-dir folder)))]
    (run! fs/delete-if-exists over)
    (count over)))

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

(def one-buffer-bytes
  "The largest file http-kit sends in one answer: it puts the body into
   one buffer, and a buffer ends at 2 GB."
  Integer/MAX_VALUE)

(defn serve
  "Answers `request` with the file at `path` of the work folder
   `:rep/dir`, for a caller that has checked who asks: the server after
   the bearer token, a host after a credential of its own. The path gets the
   checks of `file`. With a `Range` header it answers 206 and that part
   of the file, at most `range-bytes` of it, and `Content-Range` says
   which bytes came and how many the file has. Else it answers

     403  the path is no path inside the work folder
     404  there is no such file
     416  the range is malformed or names no byte of the file

   Without a `Range` the answer is the whole file, with the `File`
   itself as the body. Who writes that body decides how large the file
   may be.

   http-kit writes it by default, and http-kit sends a file up to 2 GB:
   it puts the body of an answer into one buffer, refuses a `File` above
   that with a 500, and has no way to stream with a `Content-Length`.
   So a plain GET of a larger file is answered here, with a 400 that
   names the size and says how to fetch the file in parts. A `Range`
   works for a file of any size.

   With the option `:rep/door?` the door writes it, `rep.door`, a
   listener of rep's own in front of http-kit: slice by slice, with a
   write that waits for the client, so a file of any size comes down in
   one GET with its `Content-Length`. The door marks its requests with
   `:door/request?`, which is how this function knows who writes."
  [w request path]
  (let [target (target w path)
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

          (and (< one-buffer-bytes length) (not (:door/request? request)))
          (text 400
                (str path " holds " length " bytes. This server sends a file above 2 GB in parts only."
                     " Ask with a Range header, as curl -r does: each answer carries 8 MB at most.\n"))

          :else
          {:status 200
           :headers headers
           :body target})))))

(comment
  (def w {:rep/dir "work"})
  (relative "a/../b.txt")
  (relative "../b.txt")
  (requested {:uri "/file/in/clip%20one.mp4"})
  (byte-range "bytes=0-99" 1000)
  (byte-range "bytes=900-" 1000)
  (byte-range "bytes=1000-" 1000)
  (byte-range "bytes=99999999999999999999-" 1000)
  (serve w {:headers {"range" "bytes=0-9"}} "in/clip one.mp4")
  (forget-uploads! {:rep/state-dir "work/.rep"}))
