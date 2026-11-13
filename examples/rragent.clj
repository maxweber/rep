(ns rragent
  "Remote control for the rragent that runs on this machine, as a prelude
   for REP sessions (`bb serve --prelude examples/rragent.clj`).

   A message goes in the way the page's composer sends it: the command
   `:chat/send` to rragent's own `/command`, with the cookie rragent
   derives from the machine's token. The answer is read from rragent's
   SQLite file, read only, because rragent offers no route that prints
   a conversation.

     (rragent/send! \"Make a story about our autumn sale\")
     (rragent/latest 5)
     (rragent/ask! \"Which templates do you know?\")"
  (:require [babashka.http-client :as http]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [cognitect.transit :as transit]))

(def address
  "Where rragent listens on this machine."
  (or (System/getenv "RRAGENT_ADDRESS") "http://127.0.0.1:8081"))

(def database
  "rragent's storage, opened read only."
  (or (System/getenv "RRAGENT_DATABASE") "/home/sprite/rragent/agent/agent.sqlite"))

(def bot
  "The conversation that is the bot. A machine with one loop has the id 1."
  (or (System/getenv "RRAGENT_BOT") "1"))

(defn- token
  "The page token: the sprite's label `token:<hex>`, else RRAGENT_TOKEN."
  []
  (or (some->> (try
                 (-> (process/shell {:out :string :err :string} "sprite-env" "discover")
                     :out
                     (json/parse-string true)
                     (get-in [:sprite :labels]))
                 (catch Exception _ nil))
               (some #(when (str/starts-with? % "token:") (subs % 6))))
      (System/getenv "RRAGENT_TOKEN")
      (throw (ex-info "No token: neither a sprite label token:<hex> nor RRAGENT_TOKEN." {}))))

(defn- cookie
  "The cookie rragent accepts: HMAC-SHA256 of \"rragent-cookie\" keyed
   with the token, in hex."
  []
  (let [mac (javax.crypto.Mac/getInstance "HmacSHA256")]
    (.init mac (javax.crypto.spec.SecretKeySpec. (.getBytes (token) "UTF-8") "HmacSHA256"))
    (str "rragent="
         (apply str (map #(format "%02x" %) (.doFinal mac (.getBytes "rragent-cookie" "UTF-8")))))))

(defn- to-transit
  [value]
  (let [out (java.io.ByteArrayOutputStream.)]
    (transit/write (transit/writer out :json) value)
    (.toString out "UTF-8")))

(defn- from-transit
  [text]
  (transit/read (transit/reader (java.io.ByteArrayInputStream. (.getBytes (str text) "UTF-8")) :json)))

(defn command!
  "Sends one command of rragent's page, as `{:command/kind … :command/data …}`
   does it, and answers rragent's reply."
  [kind data]
  (let [{:keys [status body]} (http/post (str address "/command")
                                         {:headers {"Cookie" (cookie)
                                                    "Content-Type" "application/transit+json"}
                                          :body (to-transit {:command/kind kind :command/data data})
                                          :throw false})]
    (if (= 200 status)
      (from-transit body)
      {:success? false :status status :body body})))

(defn send!
  "Puts `text` into the bot's chat as a person's message. Answers at once;
   the bot works on it in the background."
  [text]
  (command! :chat/send {:bot bot :text (str text)}))

(defn query
  "Runs a read-only SQL query on rragent's storage. Rows come as maps."
  [sql]
  (let [out (:out (process/shell {:out :string} "sqlite3" "-readonly" "-json" database sql))]
    (if (str/blank? out)
      []
      (json/parse-string out true))))

(defn records
  "The newest `n` entries of the bot's conversation, oldest first, each
   with its `:id` and the record as rragent stored it."
  [n]
  (->> (query (str "select id, record from entries where conversation_id = " (parse-long bot)
                   " order by id desc limit " (long n)))
       (map (fn [{:keys [id record]}]
              (assoc (json/parse-string record true) :id id)))
       (reverse)))

(defn- part-text
  "The text of one content part, a tool call as one line."
  [{:keys [type text name arguments]}]
  (case type
    "text" text
    "toolCall" (str "[tool " name " " (pr-str arguments) "]")
    nil))

(defn- message
  "One entry as `{:id :role :text}`, nil for what is no message."
  [{:keys [id model]}]
  (let [{:keys [role content toolName]} (first model)
        text (if (string? content)
               content
               (str/join "\n" (keep part-text content)))]
    (when (and (contains? #{"user" "assistant" "toolResult"} role)
               (not (str/blank? text)))
      (cond-> {:id id :role (keyword role) :text text}
        toolName (assoc :tool toolName)))))

(defn latest
  "The newest `n` messages of the bot's chat, oldest first: what the
   person said, what the bot answered, and its tool calls and results."
  ([]
   (latest 10))
  ([n]
   (->> (records (* 3 n))
        (keep message)
        (take-last n)
        (vec))))

(defn- submissions
  [where]
  (query (str "select id, status, json_extract(record, '$.answer') as answer from submissions"
              " where conversation_id = " (parse-long bot) " and " where " order by id")))

(defn busy?
  "True while the bot still works on something that was sent to it."
  []
  (boolean (seq (submissions "status in ('queued', 'placed')"))))

(defn answer
  "What became of the submission `id`: `{:status :done :text …}` once the
   bot answered, `{:status :running}` before."
  [id]
  (let [{:keys [status answer]} (first (submissions (str "id = " (long id))))]
    (if (and (= "done" status) answer)
      {:status :done
       :submission id
       :text (:text (message (first (filter #(= answer (:id %)) (records 200)))))}
      {:status :running :submission id})))

(defn ask!
  "Sends `text` and waits up to `seconds` (120) for the bot's answer.
   Answers `{:status :done :text …}`, or `{:status :running :submission id}`
   when the bot needs longer; `(rragent/answer id)` asks again later."
  ([text]
   (ask! text 120))
  ([text seconds]
   (let [before (or (:id (last (submissions "1 = 1"))) 0)
         sent (send! text)
         deadline (+ (System/currentTimeMillis) (* 1000 seconds))]
     (if-not (:success? sent)
       sent
       (loop []
         (let [mine (first (submissions (str "id > " before)))
               result (when mine (answer (:id mine)))]
           (cond
             (= :done (:status result)) result
             (> (System/currentTimeMillis) deadline) (or result {:status :running})
             :else (do (Thread/sleep 1000) (recur)))))))))

(comment
  (send! "Reply with the single word: pong")
  (busy?)
  (latest 4)
  (ask! "Which skills do you offer?" 60))
