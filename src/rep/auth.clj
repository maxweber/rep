(ns rep.auth
  "Who may call: one check. A request carries a bearer token, a JWT
   signed with Ed25519, and `check` verifies it against the public keys
   the server was configured with. That is all rep does about who
   calls. It signs nothing, keeps nothing on disk for it and holds no
   secret: whoever runs the machine hands out the tokens, see
   `rep.token` for a host on babashka.

   What `check` looks at, in this order. The first that fails refuses
   the token, and its keyword is the reason:

     :no-token       there is none
     :no-keys        the server has no public key
     :too-long       above `max-token-chars`, refused before it is parsed
     :malformed      not three parts, a part that is not base64url
                     without padding, a header that is no JSON object
     :algorithm      the header's `alg` is not exactly `EdDSA`
     :critical       the header has `crit`, and rep knows no extension
     :signature      none of the server's keys verifies the signature
     :claims         the payload is no JSON object
     :no-exp         no `exp`, or one that is no number
     :expired        `exp` does not lie ahead
     :not-yet-valid  `nbf` is there and does not lie behind
     :no-audience    no `aud`
     :audience       `aud` is not the server's `:rep/audience` and no
                     array that has it

   Of the header only `alg` and `crit` are read. `kid`, `jwk`, `jku`,
   `x5u` and `x5c` choose nothing: a key never comes from a token. The
   payload is read after the signature holds, never before.

   There is no revocation. A token is good until its `exp`. The issuer
   has two answers to a token that got out: short lifetimes, and a new
   key pair, which ends every token at once."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(def max-token-chars
  "The longest token `check` reads. A token with a handful of claims has
   some 300 characters."
  4096)

(def reasons
  "Each reason of `check` in words, for the server's own log. Nothing
   in them comes from the request."
  {:no-token "it carries no bearer token"
   :no-keys "this server has no public key (:rep/public-keys) and lets nobody in"
   :too-long "its token is too long to be read"
   :malformed "its token is no JWT of three base64url parts"
   :algorithm "its token's alg is not EdDSA"
   :critical "its token's header has crit"
   :signature "none of the public keys verifies its token's signature"
   :claims "its token's payload is no JSON object"
   :no-exp "its token has no exp"
   :expired "its token has expired"
   :not-yet-valid "its token's nbf lies ahead"
   :no-audience "its token has no aud"
   :audience "its token's aud is not this server's audience"})

(defn now
  "The current time in epoch seconds. A test holds the clock here."
  []
  (quot (System/currentTimeMillis) 1000))

(def ^:private pem
  "A public key as a PEM file has it."
  #"(?s)-----BEGIN PUBLIC KEY-----(.*?)-----END PUBLIC KEY-----")

(defn- written
  "The keys that `text` holds, each as the base64 of its
   SubjectPublicKeyInfo: what stands in a PEM file, and every other run
   of characters between commas, blanks and line breaks."
  [text]
  (let [text (str/replace (str text) "\\n" "\n")]
    (concat (map #(str/replace (second %) #"\s+" "") (re-seq pem text))
            (remove str/blank? (str/split (str/replace text pem " ") #"[\s,]+")))))

(defn- public-key
  "The Ed25519 public key that `base64` writes."
  [base64]
  (try
    (.generatePublic (java.security.KeyFactory/getInstance "Ed25519")
                     (java.security.spec.X509EncodedKeySpec. (.decode (java.util.Base64/getDecoder) (str base64))))
    (catch Exception _
      (throw (ex-info (str "Not an Ed25519 public key: " (pr-str base64) ". Give the base64 of its "
                           "SubjectPublicKeyInfo, 60 characters that start with MCowBQYDK2VwAyEA, "
                           "or the PEM file around it.")
                      {:rep/error :public-key})))))

(defn public-keys
  "The Ed25519 public keys that `given` writes, as a vector of keys for
   `check`. `given` is a text or several of them. A key is written as

   - the base64 of its X.509 SubjectPublicKeyInfo: 60 characters that
     start with `MCowBQYDK2VwAyEA`, the one line in the middle of a
     PEM file, or
   - that PEM file itself, `-----BEGIN PUBLIC KEY-----` and the lines
     after it, as `openssl pkey -pubout` prints it.

   One text holds one key or several, apart by commas, blanks or line
   breaks, so that the variable `REP_PUBLIC_KEYS` takes two keys while
   an issuer changes its own. Throws for a text that is no such key,
   and names it: a public key is no secret. What is no text is taken
   for a key that was read before."
  [given]
  (->> (if (string? given) [given] given)
       (mapcat #(if (string? %)
                  (map public-key (written %))
                  [%]))
       (vec)))

(defn- encode
  "`data` as base64url without padding."
  [data]
  (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder)) data))

(defn- decode
  "The bytes of `part` when it is base64url without padding and written
   the one way its bytes are written, else nil. Java's decoder also
   takes padding and loose last bits, which is why the bytes are
   written again and compared."
  [part]
  (when (re-matches #"[A-Za-z0-9_-]+" part)
    (try
      (let [data (.decode (java.util.Base64/getUrlDecoder) ^String part)]
        (when (= part (encode data))
          data))
      (catch IllegalArgumentException _ nil))))

(defn- object
  "The JSON object that `data` writes, or nil."
  [data]
  (let [value (try
                (json/parse-string (String. ^bytes data "UTF-8"))
                (catch Exception _ nil))]
    (when (map? value)
      value)))

(defn- verifies?
  "True when `signature` is the Ed25519 signature of `content` under
   `key`."
  [key content signature]
  (try
    (.verify (doto (java.security.Signature/getInstance "Ed25519")
               (.initVerify key)
               (.update ^bytes content))
             signature)
    (catch Exception _ false)))

(defn- fits?
  "True when `aud` names `audience`: it is that very text, or an array
   that has it."
  [audience aud]
  (boolean (and (string? audience)
                (not (str/blank? audience))
                (or (= audience aud)
                    (and (sequential? aud) (some #(= audience %) aud))))))

(defn- unfit
  "Why the claims of a token whose signature holds do not let it in, as
   a keyword, or nil when they do."
  [audience {:strs [exp nbf aud] :as claims}]
  (cond
    (not (map? claims)) :claims
    (not (number? exp)) :no-exp
    (not (< (now) exp)) :expired
    (and (contains? claims "nbf") (not (and (number? nbf) (<= nbf (now))))) :not-yet-valid
    (nil? aud) :no-audience
    (not (fits? audience aud)) :audience))

(defn check
  "Verifies `token` for the system `w`, which has `:rep/public-keys`,
   the keys of `public-keys`, and `:rep/audience`, the name of this
   machine. Answers `:auth/claims`, all claims of the token as a map
   with string keys, or `:auth/refused`, the reason as a keyword of the
   list above."
  [{:rep/keys [public-keys audience]} token]
  (let [token (str token)
        parts (when (<= (count token) max-token-chars)
                (str/split token #"\." -1))
        [header payload signature] (when (= 3 (count parts))
                                     (map decode parts))
        head (some-> header object)
        content (.getBytes (str (first parts) "." (second parts)) "US-ASCII")
        unsigned (cond
                   (str/blank? token) :no-token
                   (empty? public-keys) :no-keys
                   (nil? parts) :too-long
                   (not (and head payload signature)) :malformed
                   (not= "EdDSA" (get head "alg")) :algorithm
                   (contains? head "crit") :critical
                   (not-any? #(verifies? % content signature) public-keys) :signature)
        claims (when-not unsigned
                 (object payload))
        refused (or unsigned (unfit audience claims))]
    (if refused
      {:auth/refused refused}
      {:auth/claims claims})))

(defn claims
  "The claims of `token` when `check` lets it in, else nil."
  [w token]
  (:auth/claims (check w token)))

(defn session
  "The session that a token's `claims` name, for a request whose path
   names none: the claim `sid` when the token has one, else `sub`. Each
   is a text that is not blank. A text that is a UUID is the session's
   id as it stands. Any other text becomes one, the name-based UUID of
   its UTF-8 bytes (version 3, `java.util.UUID/nameUUIDFromBytes`), so
   the same text names the same session after every restart. Nil for
   claims with neither."
  [claims]
  (when-let [named (->> [(get claims "sid") (get claims "sub")]
                        (filter #(and (string? %) (not (str/blank? %))))
                        (first))]
    (str (or (parse-uuid named)
             (java.util.UUID/nameUUIDFromBytes (.getBytes ^String named "UTF-8"))))))

(comment
  (require '[rep.token :as token])
  (def pair (token/key-pair))
  (def w {:rep/public-keys (public-keys (:token/public-key pair))
          :rep/audience "my-machine"})
  (def token (token/sign {"aud" "my-machine" "sub" "max" "exp" (+ (now) 3600)}
                         (:token/private-key pair)))
  (check w token)
  (check w (str token "x"))
  (check (assoc w :rep/audience "another-machine") token)
  (session (claims w token))
  (session {"sid" "0b0f6a0e-4c56-4b6b-9a52-7f6f3f0f8a11"}))
