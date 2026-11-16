(ns rep.auth-test
  "The one check of who may call. The tokens of `vectors.json` were
   signed by Node's crypto (`vectors.mjs`), a second implementation
   next to the JDK's Ed25519 that rep verifies with. The clock stands
   at the second the vectors were made for."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rep.auth :as auth]
            [rep.server :as server]
            [rep.server-test :as server-test]
            [rep.token :as token]))

(def vectors
  (json/parse-string (slurp (io/resource "rep/vectors.json"))))

(defn- system
  "A system that verifies against the vectors' keys of these names."
  [names]
  {:rep/public-keys (auth/public-keys (map #(get-in vectors ["keys" %]) names))
   :rep/audience (get vectors "audience")})

(defn- at-vector-time
  "Calls `f` with the clock at the second the vectors were made for."
  [f]
  (with-redefs [auth/now (constantly (get vectors "now"))]
    (f)))

(deftest vectors-test
  (at-vector-time
   (fn []
     (doseq [{:strs [name keys token claims refused]} (get vectors "tokens")]
       (testing name
         (is (= (if refused
                  {:auth/refused (keyword refused)}
                  {:auth/claims claims})
                (auth/check (system keys) token)))))))
  (testing "the vectors have what they are here for"
    (let [tokens (get vectors "tokens")]
      (is (<= 6 (count (filter #(get % "claims") tokens))))
      (is (= #{:too-long :malformed :algorithm :critical :signature :claims :no-exp :expired :not-yet-valid
               :no-audience :audience}
             (set (keep #(some-> (get % "refused") keyword) tokens)))
          "every reason a token can be refused for")
      (is (every? #(contains? auth/reasons %) (keep #(some-> (get % "refused") keyword) tokens))))))

(deftest clock-test
  (let [{:strs [token claims]} (first (get vectors "tokens"))
        exp (get claims "exp")
        w (system ["first"])
        at #(with-redefs [auth/now (constantly %)]
              (auth/check w token))]
    (testing "a token is good until the second before its exp, and not a second longer"
      (is (= {:auth/claims claims} (at (- exp 1))))
      (is (= {:auth/refused :expired} (at exp)))
      (is (= {:auth/refused :expired} (at (+ exp 100000)))))
    (testing "the real clock lies past the vectors' exp"
      (is (= {:auth/refused :expired} (auth/check w token))))))

(deftest system-test
  (at-vector-time
   (fn []
     (let [{:strs [token claims]} (first (get vectors "tokens"))
           open (->> (get vectors "tokens")
                     (filter #(= "an aud array without this machine" (get % "name")))
                     (first))]
       (testing "nobody gets in without a token"
         (is (= {:auth/refused :no-token} (auth/check (system ["first"]) nil)))
         (is (= {:auth/refused :no-token} (auth/check (system ["first"]) ""))))
       (testing "a server without a key lets nobody in"
         (is (= {:auth/refused :no-keys} (auth/check (system []) token))))
       (testing "a server without the name of its machine lets nobody in, an aud of null does not fit it"
         (doseq [audience [nil "" " "]]
           (is (= {:auth/refused :audience}
                  (auth/check (assoc (system ["first"]) :rep/audience audience) token)))
           (is (= {:auth/refused :audience}
                  (auth/check (assoc (system ["first"]) :rep/audience audience) (get open "token"))))))
       (testing "claims answers the whole map, or nil"
         (is (= claims (auth/claims (system ["first"]) token)))
         (is (nil? (auth/claims (system ["second"]) token))))))))

(deftest public-keys-test
  (let [first-key (get-in vectors ["keys" "first"])
        second-key (get-in vectors ["keys" "second"])
        pem (get-in vectors ["keys" "first-pem"])
        token (get (first (get vectors "tokens")) "token")
        lets-in? (fn [given]
                   (at-vector-time
                    #(some? (auth/claims {:rep/public-keys (auth/public-keys given)
                                          :rep/audience (get vectors "audience")}
                                         token))))]
    (testing "the base64 of the SubjectPublicKeyInfo: 60 characters, the last of them padding that may be missing"
      (is (= 60 (count first-key)))
      (is (str/starts-with? first-key "MCowBQYDK2VwAyEA"))
      (is (lets-in? first-key))
      (is (lets-in? (subs first-key 0 59))))
    (testing "the PEM file, as openssl writes it, also with \\n written out as an env file has it"
      (is (str/starts-with? pem "-----BEGIN PUBLIC KEY-----\n"))
      (is (lets-in? pem))
      (is (lets-in? (str/replace pem "\n" "\\n")))
      (is (lets-in? (str/replace pem "\n" "\r\n"))))
    (testing "several keys in one text, apart by commas, blanks or line breaks, in either form"
      (is (= 2 (count (auth/public-keys (str second-key "," first-key)))))
      (is (lets-in? (str second-key "," first-key)))
      (is (lets-in? (str second-key "\n" first-key "\n")))
      (is (lets-in? (str second-key " , " pem)))
      (is (lets-in? (str pem second-key)))
      (is (not (lets-in? second-key))))
    (testing "several texts, and keys that were read before"
      (is (lets-in? [second-key pem]))
      (is (lets-in? (auth/public-keys [second-key first-key])))
      (is (= [] (auth/public-keys [])))
      (is (= [] (auth/public-keys "")))
      (is (= [] (auth/public-keys nil))))
    (testing "what is no Ed25519 public key throws and says which forms are taken"
      (doseq [text ["not a key"
                    (subs first-key 0 50)
                    (subs first-key 16)
                    (:token/private-key (token/key-pair))
                    ;; an X25519 public key, and the start of an RSA one
                    "MCowBQYDK2VuAyEAhSDwCYkwp1R0i33ctD73Wg2/Og0mOBr066SpjqqbTmo="
                    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA"]]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not an Ed25519 public key.*MCowBQYDK2VwAyEA"
                              (auth/public-keys text))
            text)))))

(defn- rewritten
  "The token with its last part written by `f`."
  [token f]
  (let [[header payload signature] (str/split token #"\.")]
    (str header "." payload "." (f signature))))

(deftest token-test
  (let [{:token/keys [public-key private-key] :as pair} (token/key-pair)
        w {:rep/public-keys (auth/public-keys public-key) :rep/audience "this-machine"}
        claims {"aud" "this-machine" "sub" "max" "exp" (+ (auth/now) 60) "nbf" (auth/now) "more" {"run" 7}}
        token (token/sign claims private-key)]
    (testing "a key pair is two texts, the public one in the form the server takes"
      (is (= #{:token/public-key :token/private-key} (set (keys pair))))
      (is (= 60 (count public-key)))
      (is (= 64 (count private-key)))
      (is (not= (token/key-pair) (token/key-pair))))
    (testing "what rep.token signs, rep.auth lets in, with all of its claims"
      (is (= {:auth/claims claims} (auth/check w token)))
      (is (re-matches #"[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]{86}" token)))
    (testing "the private key as a PEM file signs the same token, Ed25519 has one signature per content"
      (is (= token (token/sign claims (str "-----BEGIN PRIVATE KEY-----\n" private-key "\n-----END PRIVATE KEY-----\n")))))
    (testing "what is no private key throws and does not show what it was given"
      (let [thrown (try
                     (token/sign claims "not-a-private-key")
                     (catch clojure.lang.ExceptionInfo e e))]
        (is (= :private-key (:rep/error (ex-data thrown))))
        (is (not (str/includes? (ex-message thrown) "not-a-private-key")))))
    (testing "the same signature written another way is no token: padding, the other alphabet, loose last bits"
      (let [last-char (last (last (str/split token #"\.")))
            ;; the last character of 86 carries two bits of the signature, the other four are loose
            alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
            loose (nth alphabet (inc (str/index-of alphabet (str last-char))))]
        (is (= (seq (.decode (java.util.Base64/getUrlDecoder) (str "A" last-char)))
               (seq (.decode (java.util.Base64/getUrlDecoder) (str "A" loose))))
            "Java's decoder reads both as the same byte")
        (doseq [changed [(rewritten token #(str % "=="))
                         (rewritten token #(str (subs % 0 85) loose))
                         (rewritten token #(-> % (str/replace "-" "+") (str/replace "_" "/") (str "+")))
                         (str token "\n")
                         (str " " token)
                         (str token ".")
                         (str "." token)
                         (str/replace token "." "..")]]
          (is (= {:auth/refused :malformed} (auth/check w changed))))))
    (testing "one changed character of the signature or the payload"
      (is (= {:auth/refused :signature}
             (auth/check w (rewritten token #(str (if (= \A (first %)) "B" "A") (subs % 1))))))
      (let [[header payload signature] (str/split token #"\.")
            other (second (str/split (token/sign (assoc claims "sub" "root") private-key) #"\."))]
        (is (not= payload other))
        (is (= {:auth/refused :signature} (auth/check w (str header "." other "." signature))))))))

(deftest named-keys-test
  (let [one (token/key-pair)
        two (token/key-pair)
        kid-one (auth/key-id (:token/public-key one))
        kid-two (auth/key-id (:token/public-key two))
        listed (auth/jwks (json/generate-string {"keys" [(auth/jwk (:token/public-key one))
                                                         (auth/jwk (:token/public-key two))]}))
        w {:rep/public-keys listed :rep/audience "machine"}
        claims {"aud" "machine" "sub" "max" "exp" (+ (auth/now) 60)}
        signed (fn [pair kid] (token/sign claims (:token/private-key pair) kid))]
    (testing "the name of a key is the base64url of the first 8 bytes of the SHA-256 of its 32 raw bytes, and a JWK holds those bytes: worked out with openssl and sha256sum for the vectors' first key"
      (is (= "boWZnFUYgf8" (auth/key-id (get-in vectors ["keys" "first"]))))
      (is (= {"kty" "OKP" "crv" "Ed25519" "kid" "boWZnFUYgf8" "x" "pmWJM7J5JKNVvjKrjWmIq_0FdC6EwgCtmgfXXJGhixY"}
             (auth/jwk (get-in vectors ["keys" "first"]))))
      (is (= #{kid-one kid-two} (set (keys listed)))))
    (testing "the kid chooses the key, among the server's own keys and nowhere else"
      (is (= claims (:auth/claims (auth/check w (signed one kid-one)))))
      (is (= claims (:auth/claims (auth/check w (signed two kid-two)))))
      (is (= {:auth/refused :signature} (auth/check w (signed one kid-two))) "the key the kid names, and no other, is tried")
      (is (= {:auth/refused :unknown-key} (auth/check w (signed one "another"))))
      (is (= {:auth/refused :unknown-key} (auth/check w (token/sign claims (:token/private-key one))))
          "a token without a kid names no key")
      (is (= {:auth/refused :unknown-key} (auth/check w (signed (token/key-pair) "made-up")))))
    (testing "a function of the kid stands for a list that changes: it is asked once for a token, with the kid, and before the payload is read"
      (let [asked (atom [])
            w (assoc w :rep/public-keys (fn [kid] (swap! asked conj kid) (get listed kid)))]
        (is (= claims (:auth/claims (auth/check w (signed one kid-one)))))
        (is (= {:auth/refused :unknown-key} (auth/check w (signed one "another"))))
        (is (= [kid-one "another"] @asked))
        (is (= {:auth/refused :malformed} (auth/check w "a.b")))
        (is (= {:auth/refused :algorithm} (auth/check w (str/replace (signed one kid-one) #"^[^.]+" "eyJhbGciOiJub25lIn0"))))
        (is (= 2 (count @asked)) "a token that is refused before its signature asks for no key")))
    (testing "what is no Ed25519 key with a name is left out of a JWK set, and a text that is none throws"
      (is (= {} (auth/jwks "{\"keys\":[{\"kty\":\"RSA\",\"kid\":\"a\"},{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"AAAA\",\"kid\":\"short\"},{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"pmWJM7J5JKNVvjKrjWmIq_0FdC6EwgCtmgfXXJGhixY\"}]}")))
      (is (thrown-with-msg? Exception #"Not a JWK set" (auth/jwks "[]")))
      (is (thrown-with-msg? Exception #"Not a JWK set" (auth/jwks "no json"))))
    (testing "prepare takes named keys and a function as they are, and wants the audience with them"
      (is (identical? listed (:rep/public-keys (server/prepare {:rep/public-keys listed :rep/audience "machine"
                                                                :rep/dir (str (fs/create-temp-dir {:prefix "rep-named"}))}))))
      (is (thrown-with-msg? Exception #"needs the name" (server/prepare {:rep/public-keys (fn [_] nil)}))))))

(deftest issuer-and-claims-test
  (let [pair (token/key-pair)
        w {:rep/public-keys (auth/public-keys (:token/public-key pair))
           :rep/audience "machine"
           :rep/issuer "https://issuer.example"
           :rep/claims {"org" "org-1"}
           :rep/leeway 30}
        now 1700000000
        signed (fn [claims]
                 (token/sign (merge {"aud" "machine" "iss" "https://issuer.example" "org" "org-1"
                                     "sub" "user_01" "iat" now "exp" (+ now 60)}
                                    claims)
                             (:token/private-key pair)))
        without (fn [claim]
                  (token/sign (dissoc {"aud" "machine" "iss" "https://issuer.example" "org" "org-1"
                                       "sub" "user_01" "iat" now "exp" (+ now 60)}
                                      claim)
                              (:token/private-key pair)))
        check (fn [claims] (auth/check w (signed claims)))]
    (with-redefs [auth/now (constantly now)]
      (testing "the issuer has to be the server's, and a claim the server asks for has to have its value"
        (is (:auth/claims (check {})))
        (is (= {:auth/refused :issuer} (check {"iss" "https://another.example"})))
        (is (= {:auth/refused :issuer} (auth/check w (without "iss"))) "no iss at all")
        (is (= {:auth/refused :claim} (check {"org" "org-2"})))
        (is (= {:auth/refused :claim} (auth/check w (without "org"))) "no org at all")
        (is (= {:auth/refused :claim} (check {"org" nil}))))
      (testing "an issuer's token says who asks: sub is a text that is not blank"
        (is (= {:auth/refused :subject} (auth/check w (without "sub"))) "no sub at all")
        (doseq [sub [42 "" "  " nil true ["user_01"] {"id" "user_01"}]]
          (is (= {:auth/refused :subject} (check {"sub" sub})) (pr-str sub))))
      (testing "and when it was made: iat is a number that does not lie ahead, the leeway counted in"
        (is (= {:auth/refused :issued-at} (auth/check w (without "iat"))) "no iat at all")
        (doseq [iat ["1700000000" nil true [now]]]
          (is (= {:auth/refused :issued-at} (check {"iat" iat})) (pr-str iat)))
        (is (:auth/claims (check {"iat" (- now 3600)})) "an old one is the exp's business")
        (is (:auth/claims (check {"iat" (+ now 30)})) "a clock that lags by the leeway")
        (is (= {:auth/refused :issued-at} (check {"iat" (+ now 31)})))
        (is (= {:auth/refused :issued-at} (check {"iat" (+ now 86400)})))
        (is (= {:auth/refused :issued-at} (auth/check (dissoc w :rep/leeway) (signed {"iat" (+ now 1)})))
            "without a leeway not a second ahead"))
      (testing "a server without them reads neither, and no sub and no iat either"
        (is (:auth/claims (auth/check (dissoc w :rep/issuer :rep/claims) (signed {"iss" "anybody" "org" "any"}))))
        (is (:auth/claims (auth/check (dissoc w :rep/issuer :rep/claims)
                                      (token/sign {"aud" "machine" "exp" (+ now 60)} (:token/private-key pair)))))
        (is (:auth/claims (auth/check (dissoc w :rep/issuer :rep/claims)
                                      (token/sign {"aud" "machine" "sub" 42 "iat" "soon" "exp" (+ now 60)} (:token/private-key pair))))))
      (testing "a token counts for the leeway past its exp and no second longer"
        (is (:auth/claims (check {"exp" (- now 29)})))
        (is (= {:auth/refused :expired} (check {"exp" (- now 30)})))
        (is (= {:auth/refused :expired} (check {"exp" (- now 31)})))
        (is (= {:auth/refused :expired} (auth/check (dissoc w :rep/leeway) (signed {"exp" now}))) "without a leeway none")
        (is (:auth/claims (check {"nbf" (+ now 29)})))
        (is (= {:auth/refused :not-yet-valid} (check {"nbf" (+ now 31)})))))))

(deftest session-test
  (testing "sid names the session, else sub"
    (is (= (auth/session {"sid" "desk"}) (auth/session {"sid" "desk" "sub" "max"})))
    (is (not= (auth/session {"sid" "desk"}) (auth/session {"sub" "max"})))
    (is (= (auth/session {"sub" "max"}) (auth/session {"sub" "max" "sid" ""})))
    (is (= (auth/session {"sub" "max"}) (auth/session {"sub" "max" "sid" 7}))))
  (testing "a text becomes a UUID, the same one every time"
    (is (= "312f9128-5e04-3e09-bb4a-efef23627994" (auth/session {"sub" "laptop"})))
    (is (= 3 (.version (parse-uuid (auth/session {"sub" "max@example.com"})))))
    (is (not= (auth/session {"sub" "max"}) (auth/session {"sub" "Max"}))))
  (testing "a UUID is the session as it stands, in lower case"
    (is (= "0b0f6a0e-4c56-4b6b-9a52-7f6f3f0f8a11" (auth/session {"sid" "0B0F6A0E-4C56-4B6B-9A52-7F6F3F0F8A11"}))))
  (testing "claims that name none"
    (doseq [claims [{} {"sub" ""} {"sub" " "} {"sub" 42} {"sid" nil} nil]]
      (is (nil? (auth/session claims))))))

(defn- asked
  "What `handle` answers to a POST with `bearer` as its token, and what
   it wrote to standard error meanwhile."
  [w bearer]
  (let [err (java.io.StringWriter.)
        answer (binding [*err* err]
                 (server/handle w {:request-method :post
                                   :uri (str "/rep/" (random-uuid) "/" (random-uuid))
                                   :headers (cond-> {}
                                              bearer (assoc "authorization" (str "Bearer " bearer)))
                                   :body (java.io.ByteArrayInputStream. (.getBytes "(spit \"ran.txt\" \"x\")" "UTF-8"))}))]
    {:answer answer
     :said (str err)}))

(deftest refusal-test
  (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
        w (server/prepare {:rep/dir folder
                           :rep/public-keys (get-in vectors ["keys" "first"])
                           :rep/audience (get vectors "audience")})]
    (try
      (at-vector-time
       (fn []
         (doseq [{:strs [name keys token refused]} (get vectors "tokens")
                 :when (and refused (= ["first"] keys))]
           (testing name
             (let [{:keys [answer said]} (asked w token)]
               (testing "the caller hears the same sentence for every reason"
                 (is (= server/unauthorized answer)))
               (testing "the server's own error stream says why, and nothing of the token"
                 (is (= (str "rep: a request was refused: " (get auth/reasons (keyword refused)) "\n") said))
                 (is (not-any? #(str/includes? said %) (remove str/blank? (str/split token #"\."))))))))
         (testing "no token at all"
           (let [{:keys [answer said]} (asked w nil)]
             (is (= server/unauthorized answer))
             (is (= "rep: a request was refused: it carries no bearer token\n" said))))
         (testing "the reasons are distinct sentences"
           (is (= (count auth/reasons) (count (set (vals auth/reasons))))))
         (testing "and nothing ran"
           (is (not (fs/exists? (fs/file folder "ran.txt")))))))
      (finally
        (server/stop! w)
        (fs/delete-tree folder)))))

(deftest prepare-test
  (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
        key (get-in vectors ["keys" "first"])]
    (try
      (testing "the keys are read when the system is prepared, a text that is no key stops it there"
        (is (= 1 (count (:rep/public-keys (server/prepare {:rep/dir folder :rep/public-keys key :rep/audience "a"})))))
        (is (= 2 (count (:rep/public-keys (server/prepare {:rep/dir folder
                                                           :rep/public-keys [key (get-in vectors ["keys" "second"])]
                                                           :rep/audience "a"})))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not an Ed25519 public key"
                              (server/prepare {:rep/dir folder :rep/public-keys "nonsense" :rep/audience "a"}))))
      (testing "a system that was prepared can be prepared again"
        (let [w (server/prepare {:rep/dir folder :rep/public-keys key :rep/audience "a"})]
          (is (= (:rep/public-keys w) (:rep/public-keys (server/prepare w))))))
      (testing "keys without the name of the machine throw: aud would fit nothing"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"REP_AUDIENCE"
                              (server/prepare {:rep/dir folder :rep/public-keys key})))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"REP_AUDIENCE"
                              (server/prepare {:rep/dir folder :rep/public-keys key :rep/audience " "}))))
      (testing "without a key the system is prepared and lets nobody in, it never makes a key by itself"
        (let [w (server/prepare {:rep/dir folder})]
          (is (= [] (:rep/public-keys w)))
          (is (= server/unauthorized (:answer (asked w server-test/token))))
          (is (not (fs/exists? (fs/path folder ".rep" "secret"))))))
      (finally
        (fs/delete-tree folder)))))

(deftest trial-test
  (testing "bb serve without a public key: a key pair of this process, and one token good for a day"
    (let [made (#'server/trial {:rep/audience nil})
          claims (auth/claims made (:trial/token made))]
      (is (= #{:rep/public-keys :rep/audience :trial/token} (set (keys made))))
      (is (= {"aud" "rep" "sub" "laptop"} (select-keys claims ["aud" "sub"])))
      (is (<= (- (+ (auth/now) 86400) 5) (get claims "exp") (+ (auth/now) 86400)))
      (testing "the next start makes another key, so the token of this one is worth nothing there"
        (is (nil? (auth/claims (#'server/trial {:rep/audience nil}) (:trial/token made)))))
      (testing "and only for a server that this machine alone reaches"
        (is (every? server/local? ["127.0.0.1" "127.0.0.2" "localhost" "::1"]))
        (is (not-any? server/local? ["0.0.0.0" "192.168.1.5" "::" "machine.example" "127.0.0.1.example" "" nil])))
      (testing "it keeps the name of the machine when one is configured"
        (let [named (#'server/trial {:rep/audience "my-laptop"})]
          (is (= "my-laptop" (:rep/audience named) (get (auth/claims named (:trial/token named)) "aud"))))))))
