(ns rep.auth
  "Who may call. A secret that passes through a chat should be worth
   little, so the server hands out a login code that works once and for a
   few minutes. The caller exchanges it for a token, a JWT, and keeps
   that itself. The server keeps a record per token, and a token without
   its record is refused, which is how one is revoked.

   On disk, in the state folder:

     secret          the key that signs the tokens, made on first start
     codes/<sha256>  one file per open login code, holding when it expires
     tokens/<jti>    one file per token that is still good"
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [rep.files :as files]))

(def alphabet
  "Crockford's base32: no I, L, O or U, so a code survives being read aloud."
  "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

(defn now
  "The current time in seconds."
  []
  (quot (System/currentTimeMillis) 1000))

(defn- random-hex
  [length]
  (let [data (byte-array length)]
    (.nextBytes (java.security.SecureRandom.) data)
    (apply str (map #(format "%02x" %) data))))

(defn- sha256
  [text]
  (->> (.digest (java.security.MessageDigest/getInstance "SHA-256")
                (.getBytes (str text) "UTF-8"))
       (map #(format "%02x" %))
       (apply str)))

(defn- same?
  "Compares two strings in constant time."
  [a b]
  (java.security.MessageDigest/isEqual (.getBytes (str a) "UTF-8")
                                       (.getBytes (str b) "UTF-8")))

(defn secret!
  "The key that signs the tokens: the file `secret` in the state folder,
   made on first use and readable by its owner only. It has to outlive a
   restart, else every token would die with the server."
  [state-dir]
  (let [file (fs/path state-dir "secret")]
    (when-not (fs/exists? file)
      (fs/create-dirs state-dir)
      (spit (fs/file file) (random-hex 32))
      (fs/set-posix-file-permissions file "rw-------"))
    (str/trim (slurp (fs/file file)))))

(defn file-secret
  "The key that signs file URLs, derived from `secret`. A session gets
   this one. Nobody can turn it back into the key that signs tokens."
  [secret]
  (files/hmac secret "rep file urls"))

(defn new-code
  "A fresh login code, 60 random bits as three groups of four characters."
  []
  (let [random (java.security.SecureRandom.)]
    (->> (repeatedly 12 #(nth alphabet (.nextInt random 32)))
         (partition 4)
         (map #(apply str %))
         (str/join "-"))))

(defn normalize
  "A code as typed or pasted, brought to the form it was made in: upper
   case, no dashes or blanks, and the letters that look like digits read
   as those digits."
  [text]
  (-> (str/upper-case (str text))
      (str/replace #"[^0-9A-Z]" "")
      (str/replace "O" "0")
      (str/replace #"[IL]" "1")))

(defn- code-file
  [{:rep/keys [state-dir]} code]
  (fs/path state-dir "codes" (sha256 (normalize code))))

(defn mint-code!
  "Makes a login code that holds for `:rep/code-minutes` and answers it.
   Only its SHA-256 is kept."
  [{:rep/keys [code-minutes] :as w}]
  (let [code (new-code)
        file (code-file w code)]
    (fs/create-dirs (fs/parent file))
    (spit (fs/file file) (pr-str {:expires (+ (now) (* 60 code-minutes))}))
    code))

(defn- claim-code!
  "True when `code` is open and not expired. Either way it is gone
   afterwards. The delete is what makes it work once: of two callers with
   the same code only one deletes the file."
  [w code]
  (let [file (code-file w code)
        record (when (fs/exists? file)
                 (try
                   (edn/read-string (slurp (fs/file file)))
                   (catch Exception _ nil)))]
    (boolean (and (fs/delete-if-exists file)
                  (some-> (:expires record) (> (now)))))))

(defn- encode
  "`data` as JSON in URL-safe Base64 without padding."
  [data]
  (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder))
                   (.getBytes (json/generate-string data) "UTF-8")))

(defn- decode
  [part]
  (json/parse-string (String. (.decode (java.util.Base64/getUrlDecoder) (str part)) "UTF-8")))

(defn sign
  "A JWT with `claims`, signed with HMAC-SHA256 under the server's key."
  [{:rep/keys [secret]} claims]
  (let [content (str (encode {"alg" "HS256" "typ" "JWT"}) "." (encode claims))]
    (str content "." (files/hmac secret content))))

(defn- token-file
  "The record of the token `jti`, or nil when `jti` is no UUID."
  [{:rep/keys [state-dir]} jti]
  (when (parse-uuid (str jti))
    (fs/path state-dir "tokens" (str jti))))

(defn issue!
  "A new token that holds for `:rep/token-days`, and its record."
  [{:rep/keys [token-days] :as w}]
  (let [issued (now)
        claims {"iss" "rep"
                "iat" issued
                "exp" (+ issued (* 86400 token-days))
                "jti" (str (random-uuid))}
        file (token-file w (get claims "jti"))]
    (fs/create-dirs (fs/parent file))
    (spit (fs/file file) (pr-str {:jti (get claims "jti")
                                  :issued issued
                                  :expires (get claims "exp")}))
    (sign w claims)))

(defn exchange!
  "The token for a login code, or nil for a code that is unknown, used or
   expired."
  [w code]
  (when (claim-code! w code)
    (issue! w)))

(defn claims
  "The claims of `token` when it is one of ours and still good: the
   signature fits, it has not expired and its record is there. Else nil."
  [{:rep/keys [secret] :as w} token]
  (try
    (let [[header payload signature & more] (str/split (str token) #"\.")
          content (str header "." payload)]
      (when (and header payload signature (not more)
                 (same? signature (files/hmac secret content))
                 (= "HS256" (get (decode header) "alg")))
        (let [{:strs [iss exp jti] :as claims} (decode payload)]
          (when (and (= "rep" iss)
                     (number? exp)
                     (> exp (now))
                     (some-> (token-file w jti) fs/exists?))
            claims))))
    (catch Exception _ nil)))

(defn revoke!
  "Removes the record of the token `jti`. True when there was one."
  [w jti]
  (boolean (some-> (token-file w jti) fs/delete-if-exists)))

(defn tokens
  "The records of the tokens that are still good, oldest first."
  [{:rep/keys [state-dir]}]
  (let [folder (fs/path state-dir "tokens")]
    (when (fs/exists? folder)
      (->> (fs/list-dir folder)
           (keep #(try
                    (edn/read-string (slurp (fs/file %)))
                    (catch Exception _ nil)))
           (filter #(> (:expires %) (now)))
           (sort-by :issued)))))

(comment
  (def w {:rep/state-dir "work/.rep" :rep/code-minutes 10 :rep/token-days 30
          :rep/secret (secret! "work/.rep")})
  (def code (mint-code! w))
  (def token (exchange! w code))
  (exchange! w code)
  (claims w token)
  (tokens w)
  (revoke! w (get (claims w token) "jti")))
