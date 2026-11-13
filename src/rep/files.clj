(ns rep.files
  "The work folder, and signed URLs that move one file into it or out of
   it. A signed URL needs no token. Its signature names the method, the
   path and the moment it expires, so it can be handed to another tool."
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
  "What signs `method` on `path` until the second `expires`."
  [{:rep/keys [secret]} method path expires]
  (hmac secret (str method "\n" path "\n" expires)))

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

(defn- text
  [status body]
  {:status status
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body body})

(defn store!
  "Answers a signed PUT. The body becomes the file."
  [w {:keys [body] :as request}]
  (let [{:file/keys [path refused] :as w} (check w request)]
    (if refused
      (text 403 (str refused "\n"))
      (let [target (file w path)]
        (io/make-parents target)
        (with-open [out (io/output-stream target)]
          (when body
            (io/copy body out)))
        (text 201 (str "Stored " path " (" (fs/size target) " bytes).\n"))))))

(defn fetch
  "Answers a signed GET with the file."
  [w request]
  (let [{:file/keys [path refused] :as w} (check w request)
        target (when path (file w path))]
    (cond
      refused (text 403 (str refused "\n"))
      (not (fs/regular-file? target)) (text 404 (str "No file " path ".\n"))
      :else {:status 200
             :headers {"Content-Type" "application/octet-stream"}
             :body target})))

(comment
  (def w {:rep/dir "work" :rep/secret "secret"})
  (relative "a/../b.txt")
  (relative "../b.txt")
  (url w {:url/method "PUT" :url/path "in/clip one.mp4" :url/seconds 900
          :url/base "http://127.0.0.1:8730"}))
