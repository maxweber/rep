(ns rep.token
  "Signing tokens. This is NOT part of the server. rep verifies a token
   against a public key (`rep.auth`) and never signs one, so the server
   needs no private key and holds no secret. Signing belongs to whoever
   hands the tokens to the callers: the host.

   Three need it on babashka, and for them it is here: a host that
   issues the tokens of its machine, the tests, and `bb serve` for a
   try on a laptop.

   A token is a JWT signed with Ed25519, `alg` EdDSA. `rep.auth` says
   which claims rep reads. Every other tool that signs such a JWT is an
   issuer as good as this one, `test/rep/vectors.mjs` does it with
   Node."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn- base64
  [data]
  (.encodeToString (java.util.Base64/getEncoder) data))

(defn key-pair
  "A new Ed25519 key pair as two texts. `:token/public-key` is the
   base64 of its SubjectPublicKeyInfo, 60 characters, which is what
   the server takes as `:rep/public-keys`. `:token/private-key` is the
   base64 of its PKCS#8, 64 characters, which is what `sign` takes.
   Whoever has the private key makes tokens the server accepts, so it
   stays with the issuer and is never printed."
  []
  (let [pair (.generateKeyPair (java.security.KeyPairGenerator/getInstance "Ed25519"))]
    {:token/public-key (base64 (.getEncoded (.getPublic pair)))
     :token/private-key (base64 (.getEncoded (.getPrivate pair)))}))

(defn- read-key
  "The private key that `written` writes: the base64 of a PKCS#8, or
   the PEM file around it (`-----BEGIN PRIVATE KEY-----`), as `openssl
   genpkey -algorithm ed25519` writes it."
  [written]
  (let [bare (-> (str written)
                 (str/replace #"-----(BEGIN|END) PRIVATE KEY-----" "")
                 (str/replace #"\s+" ""))]
    (try
      (.generatePrivate (java.security.KeyFactory/getInstance "Ed25519")
                        (java.security.spec.PKCS8EncodedKeySpec. (.decode (java.util.Base64/getDecoder) bare)))
      (catch Exception _
        (throw (ex-info "Not an Ed25519 private key. Give the base64 of its PKCS#8 or its PEM file."
                        {:rep/error :private-key}))))))

(defn- encode
  "`data` as base64url without padding."
  [data]
  (.encodeToString (.withoutPadding (java.util.Base64/getUrlEncoder)) data))

(defn sign
  "A token with `claims`, a map with string keys, signed with
   `private-key`, the text `key-pair` answers under
   `:token/private-key`.

   The server refuses a token without `exp`, a time in epoch seconds
   that lies ahead, and without `aud`, the server's `:rep/audience` or
   an array that has it. `sid`, else `sub`, names the session of a
   POST to /rep. A token cannot be revoked, so give it the shortest
   life that does the job.

   `kid` names the key in the token's header, for a server whose keys
   have names (`rep.auth/key-id` of the public key)."
  ([claims private-key]
   (sign claims private-key nil))
  ([claims private-key kid]
   (let [content (str (encode (.getBytes (json/generate-string (cond-> {"alg" "EdDSA" "typ" "JWT"}
                                                                 kid (assoc "kid" kid)))
                                         "UTF-8"))
                      "."
                      (encode (.getBytes (json/generate-string claims) "UTF-8")))
         signer (doto (java.security.Signature/getInstance "Ed25519")
                  (.initSign (read-key private-key))
                  (.update (.getBytes content "US-ASCII")))]
     (str content "." (encode (.sign signer))))))

(comment
  (def pair (key-pair))
  (:token/public-key pair)
  (def token (sign {"aud" "my-machine"
                    "sub" "max"
                    "exp" (+ (quot (System/currentTimeMillis) 1000) 3600)}
                   (:token/private-key pair)))
  (count token))
