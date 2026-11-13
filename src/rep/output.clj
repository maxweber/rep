(ns rep.output
  "The output of one evaluation. Every chunk goes into a file and to the
   HTTP channels that follow the evaluation at that moment. The file is
   what lets a second request with the same id get the same text again."
  (:require [clojure.java.io :as io]
            [org.httpkit.server :as http]))

(defn open
  "The output that appends to `file` and stops at `limit` bytes."
  [file limit]
  {:output/file (io/file file)
   :output/stream (java.io.FileOutputStream. (io/file file) true)
   :output/limit limit
   :output/lock (Object.)
   :output/state (atom {:size 0 :listeners #{} :full? false :closed? false})})

(defn read-from
  "The bytes of `file` from byte `from` on."
  [file from]
  (with-open [in (io/input-stream file)]
    (.skip in from)
    (.readAllBytes in)))

(defn- append!
  "Writes the chunk to the file and to the listeners, under the lock."
  [{:output/keys [stream state]} data offset length]
  (.write stream data offset length)
  (.flush stream)
  (when-let [listeners (seq (:listeners @state))]
    (let [chunk (byte-array length)]
      (System/arraycopy data offset chunk 0 length)
      (doseq [channel listeners]
        (http/send! channel chunk false))))
  (swap! state update :size + length))

(defn- refusal
  "The exception of an output that passed its limit."
  [limit]
  (ex-info (str "The output passed its limit of " limit " bytes.")
           {:rep/error :output-limit}))

(defn write!
  "Appends `length` bytes of `data` from `offset`. A chunk that would pass
   the limit is replaced by one line that says so, and this call and every
   later one throws. A closed output drops what it gets."
  [{:output/keys [limit lock state] :as output} data offset length]
  (locking lock
    (let [{:keys [size full? closed?]} @state]
      (cond
        closed? nil
        full? (throw (refusal limit))
        (> (+ size length) limit)
        (let [note (.getBytes (str "\n" (ex-message (refusal limit)) "\n") "UTF-8")]
          (append! output note 0 (alength note))
          (swap! state assoc :full? true)
          (throw (refusal limit)))
        :else (append! output data offset length))
      nil)))

(defn follow!
  "Sends the output from byte `from` on to `channel`, then every later
   chunk. Closes the channel at once when the output is closed already."
  [{:output/keys [file lock state]} channel from]
  (locking lock
    (let [data (read-from file from)]
      (when (pos? (alength data))
        (http/send! channel data false))
      (if (:closed? @state)
        (http/close channel)
        (swap! state update :listeners conj channel))
      nil)))

(defn leave!
  "Takes `channel` off the listeners, for a client that went away."
  [{:output/keys [lock state]} channel]
  (locking lock
    (swap! state update :listeners disj channel)
    nil))

(defn close!
  "Closes the file and the channels of everyone who follows."
  [{:output/keys [stream lock state]}]
  (locking lock
    (when-not (:closed? @state)
      (.close stream)
      (doseq [channel (:listeners @state)]
        (http/close channel))
      (swap! state assoc :closed? true :listeners #{}))
    nil))

(comment
  (def output (open "/tmp/rep-output.txt" 1000))
  (let [data (.getBytes "hello\n" "UTF-8")]
    (write! output data 0 (alength data)))
  (String. (read-from "/tmp/rep-output.txt" 2) "UTF-8")
  (close! output))
