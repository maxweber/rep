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
     :unknown-key    the server's keys have names, and the header's
                     `kid` names none of them
     :signature      the key does not verify the signature: the one
                     the `kid` names, or every key of a server whose
                     keys have no names
     :claims         the payload is no JSON object
     :no-exp         no `exp`, or one that is no number
     :expired        `exp` does not lie ahead, `:rep/leeway` counted in
     :not-yet-valid  `nbf` is there and does not lie behind
     :no-audience    no `aud`
     :audience       `aud` is not the server's `:rep/audience` and no
                     array that has it
     :issuer         the server has `:rep/issuer`, and `iss` is not it
     :subject        the server has `:rep/issuer`, and `sub` is no text
                     or a blank one
     :issued-at      the server has `:rep/issuer`, and `iat` is no
                     number or lies ahead, `:rep/leeway` counted in
     :claim          the server has `:rep/claims`, and a claim it names
                     has another value or is missing

   Of the header `alg` and `crit` are read, and `kid` by a server
   whose keys have names: it chooses one of the server's own keys and
   nothing else. `jwk`, `jku`, `x5u` and `x5c` choose nothing: a key
   never comes from a token. The payload is read after the signature
   holds, never before.

   A server with `:rep/issuer` takes the tokens of one issuer, and such
   a token says who asks and when it was made: `sub` and `iat` are
   asked for then. A server without an issuer reads neither, and a
   token of its own may leave them out.

   The keys, `:rep/public-keys`, are one of three:

     a vector of keys   any of them may verify (`public-keys`)
     a map              a key's name, its `kid`, to the key (`jwks`)
     a function         of a `kid`, which answers the key or nil: for a
                        host whose list comes from an address and
                        changes. The host fetches and keeps the list

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
   :unknown-key "its token's kid names no key of this server"
   :signature "no public key of this server verifies its token's signature"
   :claims "its token's payload is no JSON object"
   :no-exp "its token has no exp"
   :expired "its token has expired"
   :not-yet-valid "its token's nbf lies ahead"
   :no-audience "its token has no aud"
   :audience "its token's aud is not this server's audience"
   :issuer "its token's iss is not this server's issuer"
   :subject "its token's sub is no text that names who asks"
   :issued-at "its token's iat is no number, or lies ahead"
   :claim "a claim of its token is not what this server asks for"})

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
   for a key that was read before. A map of names to keys (`jwks`) and
   a function of a name are answered as they are."
  [given]
  (if (or (map? given) (fn? given))
    given
    (->> (if (string? given) [given] given)
         (mapcat #(if (string? %)
                    (map public-key (written %))
                    [%]))
         (vec))))

(defn- encode
  "`data` as base64url without padding."
  [data]
  (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder)) data))

(def ^:private spki-head
  "What stands before the 32 bytes of an Ed25519 public key in its
   SubjectPublicKeyInfo."
  (byte-array [0x30 0x2a 0x30 0x05 0x06 0x03 0x2b 0x65 0x70 0x03 0x21 0x00]))

(defn- raw
  "The 32 bytes of the Ed25519 public key `key`."
  [key]
  (let [^bytes encoded (.getEncoded key)]
    (java.util.Arrays/copyOfRange encoded (- (alength encoded) 32) (alength encoded))))

(defn key-id
  "The name of the public key `key`, a key of `public-keys` or a text
   that writes one: the base64url, without padding, of the first 8
   bytes of the SHA-256 of its 32 raw bytes. An issuer puts it into a
   token's header as `kid`."
  [key]
  (let [key (if (string? key) (first (public-keys key)) key)
        digest (.digest (java.security.MessageDigest/getInstance "SHA-256") (raw key))]
    (encode (java.util.Arrays/copyOf digest 8))))

(defn jwks
  "The keys of a JWK set as a map of each key's `kid` to the key, for
   `:rep/public-keys`. `text` is the JSON an issuer publishes:

     {\"keys\":[{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"kid\":\"...\",\"x\":\"...\"}]}

   `x` is the base64url of the key's 32 raw bytes. An entry of another
   kind, without a `kid` or with an `x` that is no such key is left
   out. Throws for a text that is no JSON object with `keys`."
  [text]
  (let [listed (try
                 (get (json/parse-string (str text)) "keys")
                 (catch Exception _ nil))]
    (when-not (sequential? listed)
      (throw (ex-info "Not a JWK set: no JSON object with the array \"keys\"." {:rep/error :jwks})))
    (into {}
          (keep (fn [{:strs [kty crv kid x]}]
                  (when (and (= "OKP" kty) (= "Ed25519" crv) (string? kid) (not (str/blank? kid)) (string? x))
                    (when-let [bytes (try
                                       (.decode (java.util.Base64/getUrlDecoder) ^String x)
                                       (catch IllegalArgumentException _ nil))]
                      (when (= 32 (alength ^bytes bytes))
                        [kid (.generatePublic (java.security.KeyFactory/getInstance "Ed25519")
                                              (java.security.spec.X509EncodedKeySpec.
                                               (byte-array (concat spki-head bytes))))])))))
          listed)))

(defn jwk
  "The public key `key` as an entry of a JWK set, with its `key-id` as
   `kid`: what an issuer publishes for `jwks` to read."
  [key]
  (let [key (if (string? key) (first (public-keys key)) key)]
    {"kty" "OKP" "crv" "Ed25519" "kid" (key-id key) "x" (encode (raw key))}))

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
   a keyword, or nil when they do. `w` is the system of `check`."
  [{:rep/keys [audience issuer leeway] required :rep/claims} {:strs [exp nbf aud iss sub iat] :as claims}]
  (let [leeway (if (number? leeway) leeway 0)]
    (cond
      (not (map? claims)) :claims
      (not (number? exp)) :no-exp
      (not (< (now) (+ exp leeway))) :expired
      (and (contains? claims "nbf") (not (and (number? nbf) (<= nbf (+ (now) leeway))))) :not-yet-valid
      (nil? aud) :no-audience
      (not (fits? audience aud)) :audience
      (and (some? issuer) (not= issuer iss)) :issuer
      (and (some? issuer) (not (and (string? sub) (not (str/blank? sub))))) :subject
      (and (some? issuer) (not (and (number? iat) (<= iat (+ (now) leeway))))) :issued-at
      (some (fn [[claim value]] (not= value (get claims claim ::none))) required) :claim)))

(defn- unsigned
  "Why the signature of a token does not hold under the keys `keys` of
   the system, as a keyword, or nil when it does. `head` is the token's
   header, `content` what was signed."
  [keys head content signature]
  (if (or (map? keys) (fn? keys))
    (let [kid (get head "kid")
          key (when (string? kid) (keys kid))]
      (cond
        (nil? key) :unknown-key
        (not (verifies? key content signature)) :signature))
    (when (not-any? #(verifies? % content signature) keys)
      :signature)))

(defn check
  "Verifies `token` for the system `w`, which has `:rep/public-keys`,
   the keys, see the namespace, and `:rep/audience`, the name of this
   machine. It may have `:rep/issuer`, what a token's `iss` has to be:
   then `sub` has to be a text that is not blank and `iat` a number
   that does not lie ahead,
   `:rep/claims`, a map of more claims to the value each has to have,
   and `:rep/leeway`, for how many seconds past its `exp` a token still
   counts, for a machine whose clock lags after it woke. Answers
   `:auth/claims`, all claims of the token as a map with string keys,
   or `:auth/refused`, the reason as a keyword of the list above."
  [{:rep/keys [public-keys] :as w} token]
  (let [token (str token)
        parts (when (<= (count token) max-token-chars)
                (str/split token #"\." -1))
        [header payload signature] (when (= 3 (count parts))
                                     (map decode parts))
        head (some-> header object)
        content (.getBytes (str (first parts) "." (second parts)) "US-ASCII")
        unsigned (cond
                   (str/blank? token) :no-token
                   (and (not (fn? public-keys)) (empty? public-keys)) :no-keys
                   (nil? parts) :too-long
                   (not (and head payload signature)) :malformed
                   (not= "EdDSA" (get head "alg")) :algorithm
                   (contains? head "crit") :critical
                   :else (unsigned public-keys head content signature))
        claims (when-not unsigned
                 (object payload))
        refused (or unsigned (unfit w claims))]
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
