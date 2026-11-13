(ns rep.nrepl
  "The client side of nREPL, as much of it as REP needs: a socket to a
   server on this machine, bencode messages out, bencode messages in."
  (:require [bencode.core :as bencode]))

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

(defn receive!
  "Waits for the next message. Throws when the server is gone."
  [{:nrepl/keys [in]}]
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

(comment
  ;; bb nrepl-server 127.0.0.1:1667
  (def connection (connect 1667))
  (def session (clone! connection))
  (call! connection {"op" "eval" "session" session "code" "(println \"hi\") (+ 1 2)"})
  (close! connection))
