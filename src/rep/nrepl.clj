(ns rep.nrepl
  "The client side of nREPL, as much of it as REP needs: a socket to a
   server on this machine, bencode messages out, bencode messages in."
  (:require [bencode.core :as bencode]
            [clojure.string :as str]))

(defn- decode
  "A bencode value with its byte strings as text and its keys as keywords."
  [value]
  (cond
    (bytes? value) (String. value "UTF-8")
    (map? value) (into {} (map (fn [[k v]] [(keyword (decode k)) (decode v)])) value)
    (sequential? value) (mapv decode value)
    :else value))

(defn connect
  "A connection to the nREPL server on `port` of this machine."
  [port]
  (let [socket (java.net.Socket. "127.0.0.1" (int port))]
    (.setTcpNoDelay socket true)
    {:nrepl/socket socket
     :nrepl/in (java.io.PushbackInputStream. (.getInputStream socket))
     :nrepl/out (java.io.BufferedOutputStream. (.getOutputStream socket))}))

(defn send!
  "Sends one message, a map with string keys."
  [{:nrepl/keys [out]} message]
  (locking out
    (bencode/write-bencode out message)
    (.flush out))
  nil)

(defonce ^{:private true
           :doc "The socket option TCP_QUICKACK of Linux, nil where there is
  none. Babashka has no name for it, so the delay picks it from the
  options a socket supports."}
  quick-ack
  (delay
    (with-open [socket (java.net.Socket.)]
      (some #(when (= "TCP_QUICKACK" (str %)) %)
            (.supportedOptions socket)))))

(defn- quick-ack!
  "Makes the kernel acknowledge at once what arrives next on
   `connection`. Linux else holds the acknowledgement of a message back
   for 40 ms, and babashka's nREPL waits for it before it sends the next
   one, the `done` after a value. That made every eval take 40 ms longer
   than its code. The setting does not stick, so `receive!` sets it
   before every message. Does nothing where the option is missing."
  [{:nrepl/keys [socket]}]
  (when-let [option @quick-ack]
    (.setOption socket option true))
  nil)

(defn receive!
  "Waits for the next message. Throws when the server is gone."
  [{:nrepl/keys [in] :as connection}]
  (quick-ack! connection)
  (decode (bencode/read-bencode in)))

(defn done?
  [message]
  (boolean (some #{"done"} (:status message))))

(defn call!
  "Sends `message` under an id of its own and collects its answers up to
   the one that says done."
  [connection message]
  (let [id (str (random-uuid))]
    (send! connection (assoc message "id" id))
    (loop [answers []]
      (let [answer (receive! connection)]
        (cond
          (not= id (:id answer)) (recur answers)
          (done? answer) (conj answers answer)
          :else (recur (conj answers answer)))))))

(defn clone!
  "A new nREPL session on the server. Sessions share every namespace and
   run at the same time, which two evals of one session do not."
  [connection]
  (some :new-session (call! connection {"op" "clone"})))

(defn close!
  [{:nrepl/keys [socket]}]
  (try
    (.close socket)
    (catch java.io.IOException _ nil)))

(defn wrapped
  "`body` as one form for the server. `load-string` stops at the first
   error, answers the value of the last form and counts lines as the
   caller wrote them. Plain nREPL would go on after an error. The
   namespace that `body` switches to does not stay either.

   With `base-url` the form binds `rep.prelude/*base-url*` to it while
   the body runs. That is where the caller reached the server, and the
   URLs that `upload-url` and `download-url` hand out start there. The
   binding belongs to this one evaluation, so two callers that reach
   the server under two addresses never get each other's."
  ([body]
   (str "(load-string " (pr-str body) ")"))
  ([body base-url]
   (if base-url
     (str "(binding [rep.prelude/*base-url* " (pr-str (str base-url)) "] " (wrapped body) ")")
     (wrapped body))))

(defn converse!
  "Sends `code` as the eval `id` in the nREPL session `:nrepl/session`
   of `connection` and reads the answers until it is done. Puts every
   chunk the code printed into the atom `chunks`, as `[:out text]` or
   `[:err text]`. Answers `:value`, the printed value of the last form,
   and `:failed?`, true when the eval ended with an error or was
   interrupted. Printing never makes an eval fail. A read past
   `deadline`, in epoch milliseconds, throws a
   `java.net.SocketTimeoutException`."
  [{:nrepl/keys [socket session] :as connection} id code deadline chunks]
  (send! connection {"op" "eval"
                     "id" id
                     "session" session
                     "code" code})
  (loop [result {}]
    (.setSoTimeout socket (int (max 1 (- deadline (System/currentTimeMillis)))))
    (let [{:keys [out err value status] :as message} (receive! connection)]
      (if (not= id (:id message))
        (recur result)
        (let [_ (when out (swap! chunks conj [:out out]))
              _ (when err (swap! chunks conj [:err err]))
              result (cond-> result
                       value (assoc :value value)
                       (some #{"eval-error" "interrupted"} status) (assoc :failed? true))]
          (if (done? message)
            result
            (recur result)))))))

(defn- printed
  "The text of `chunks` in the order they came."
  [chunks]
  (apply str (map second chunks)))

(defn outcome
  "What an eval came to, from the `chunks` it printed and the `result`
   of `converse!`: `:out`, everything the code printed, standard error
   included, `:value` and `:error`. Babashka writes its error text to
   standard error right before it reports the failure, so for an eval
   that failed the last chunk from there is the error and not output."
  [chunks {:keys [value failed?]}]
  (if-not failed?
    {:out (printed chunks) :value value :error nil}
    (let [index (->> chunks
                     (keep-indexed (fn [index [kind]] (when (= :err kind) index)))
                     (last))]
      {:out (printed (keep-indexed (fn [i chunk] (when (not= i index) chunk)) chunks))
       :value nil
       :error (or (some-> index chunks second str/trim not-empty)
                  "The evaluation failed.")})))

(comment
  ;; bb nrepl-server 127.0.0.1:1667
  (def connection (connect 1667))
  (def session (clone! connection))
  (call! connection {"op" "eval" "session" session "code" "(println \"hi\") (+ 1 2)"})
  (call! connection {"op" "eval" "session" session "code" (wrapped "(ns other) (/ 1 0) :never")})
  (wrapped "(upload-url \"in/a.txt\")" "https://machine.example")

  (def chunks (atom []))
  (def result (converse! (assoc connection :nrepl/session session)
                         "e1"
                         (wrapped "(println \"hi\") (/ 1 0)")
                         (+ (System/currentTimeMillis) 5000)
                         chunks))
  (outcome @chunks result)
  (close! connection))
