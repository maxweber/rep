(ns rep.image
  "The image, one babashka process for all sessions. With the option
   `:rep/shared-image?` every session uuid evaluates in the same process,
   so a function that one caller redefines is redefined for all. A host
   keeps a live application in it.

   The image starts with the server, not with the first request. A
   supervisor watches over it. When the process ends, for whatever
   reason, the supervisor starts a new one. Every start that works
   counts the generation up. A session uuid that evaluated in an earlier
   generation learns from `Rep-Session: restarted` that its definitions
   are gone.

   The atom `:rep/image` holds the state.

     :image/generation  how many times the image started so far
     :image/entry       a promise of the running image, or of the reason
                        why none runs
     :image/starting    the process of a start that is under way
     :image/seen        session uuid -> generation of its last evaluation
     :image/stop        a promise that `stop!` delivers
     :image/supervisor  the future that watches over the process

   A running image is what `rep.session/prepare!` answers, with
   `:image/generation`, `:image/pid` and `:image/connections`, the open
   connections that `evaluate` keeps for its next calls."
  (:require [clojure.string :as str]
            [rep.hook :as hook]
            [rep.nrepl :as nrepl]
            [rep.session :as session]))

(def stable-ms
  "An image that ran this long started well. One that ended sooner counts
   like a start that failed, and the pause before the next start grows."
  5000)

(def start-timeout-ms
  "How long an HTTP evaluation waits for an image that is starting."
  30000)

(def idle-connections
  "How many connections `evaluate` keeps open between its calls."
  4)

(defn- report!
  "Prints a line about the image to standard error."
  [& words]
  (binding [*out* *err*]
    (println (str/join " " (cons "rep:" words)))))

(defn- shared!
  "Throws when the system `w` runs without the shared image."
  [{:rep/keys [shared-image?]} caller]
  (when-not shared-image?
    (throw (ex-info (str caller " needs the shared image. Set the option :rep/shared-image? to true.")
                    {:rep/error :no-shared-image}))))

(defn pause-ms
  "How long the supervisor waits before the next start, after `failures`
   starts in a row that failed or whose image ended at once. It does not
   wait after none, `:rep/backoff-ms` after the first, then twice as
   long each time, up to `:rep/backoff-max-ms`."
  [{:rep/keys [backoff-ms backoff-max-ms]} failures]
  (if (pos? failures)
    (long (min backoff-max-ms
               (* backoff-ms (Math/pow 2 (dec (min failures 30))))))
    0))

(defn- description
  "What a host gets to know about a running image."
  [{:image/keys [generation pid] :session/keys [port start-ms]}]
  {:image/generation generation
   :image/pid pid
   :image/port port
   :image/start-ms start-ms})

(defn- attempt!
  "One try to start the image's process and to run the setup code in it.
   Answers what `rep.session/prepare!` answers. An exception becomes a
   `:session/error` as well. While the try is under way, the process is
   in `:image/starting`, so that `stop!` can end a start that hangs."
  [{:rep/keys [image] :as w} stop]
  (try
    (let [begun (System/currentTimeMillis)
          child (session/spawn! w)]
      (swap! image assoc :image/starting child)
      (try
        (if (realized? stop)
          (session/end! child)
          (session/prepare! w "image" child begun))
        (finally
          (swap! image dissoc :image/starting))))
    (catch Throwable e
      {:session/error (str "The image did not start: "
                           (or (ex-message e) (.getName (class e))))})))

(defn- publish!
  "Puts `value`, a running image or a `:session/error`, where everyone
   who asks for the image finds it. Whoever waits for the image gets it
   now."
  [{:rep/keys [image]} value]
  (let [entry (:image/entry @image)]
    (if (realized? entry)
      (swap! image assoc :image/entry (doto (promise) (deliver value)))
      (deliver entry value)))
  nil)

(defn- retire!
  "Clears away the image `running`, whose process has ended or has to
   end now. Whoever asks for the image from here on waits for the next
   one."
  [{:rep/keys [image]} running]
  (swap! image assoc :image/entry (promise))
  (session/end! (:session/process running))
  (let [[idle _] (reset-vals! (:image/connections running) nil)]
    (run! nrepl/close! idle)))

(defn- await-end!
  "Waits until the process of `running` has ended or `stop` is
   delivered. True when the process ended."
  [running stop]
  (let [process (:proc (:session/process running))]
    (loop []
      (cond
        (.waitFor process 100 java.util.concurrent.TimeUnit/MILLISECONDS) true
        (realized? stop) false
        :else (recur)))))

(defn- supervise!
  "The life of the supervisor: starts the image, waits for the end of
   its process and starts the next one, until `stop` is delivered. After
   a start that failed, or an image that ended at once, it pauses before
   the next try, so that a prelude that throws does not spin. Delivers
   `settled` when the first start is through, whether it worked or not."
  [{:rep/keys [image] :as w} stop settled]
  (try
    (loop [failures 0]
      (when-not (realized? stop)
        (let [begun (System/currentTimeMillis)
              started (attempt! w stop)]
          (cond
            (realized? stop)
            (some-> (:session/process started) (session/end!))

            (:session/error started)
            (let [failures (inc failures)
                  pause (pause-ms w failures)]
              (report! (:session/error started) (str "Next try in " pause " ms."))
              (publish! w (assoc started :image/failures failures))
              (deliver settled true)
              (deref stop pause nil)
              (recur failures))

            :else
            (let [generation (:image/generation (swap! image update :image/generation inc))
                  running (assoc started
                                 :image/generation generation
                                 :image/pid (.pid (:proc (:session/process started)))
                                 :image/connections (atom []))]
              (publish! w running)
              (hook/call! w :rep/on-image-start (description running))
              (deliver settled true)
              (let [ended? (await-end! running stop)
                    failures (if (< (- (System/currentTimeMillis) begun) stable-ms)
                               (inc failures)
                               0)]
                (retire! w running)
                (when (and ended? (not (realized? stop)))
                  (report! (str "The image's process ended, generation " generation ". Starting the next one."))
                  (deref stop (pause-ms w failures) nil))
                (recur failures)))))))
    (catch Throwable e
      (report! "The supervisor of the image broke:" (or (ex-message e) (.getName (class e)))))
    (finally
      (publish! w {:session/error "The image is stopped."})
      (deliver settled true))))

(defn- supervised?
  [{:rep/keys [image]}]
  (boolean (some-> (:image/supervisor @image) (future-done?) (not))))

(defn start!
  "Starts the image and its supervision, and waits until the first start
   is through, the hook `:rep/on-image-start` included. A first start
   that fails does not throw: the supervisor tries again, and `status`
   tells why. A setup that does not finish within `:rep/setup-ms` is
   such a failed start. Answers `w`.

   `rep.server/start!` calls this. A host that owns the HTTP server and
   only mounts `rep.server/handle` calls it on the prepared system
   itself, and `rep.server/stop!` at the end. Throws without
   `:rep/shared-image?`."
  [{:rep/keys [image] :as w}]
  (shared! w "rep.image/start!")
  (let [settled (promise)]
    (locking image
      (if (supervised? w)
        (deliver settled true)
        (let [stop (promise)]
          (swap! image assoc :image/entry (promise) :image/stop stop)
          (swap! image assoc :image/supervisor (future (supervise! w stop settled))))))
    (deref settled (+ 20000 (or (:rep/setup-ms w) session/setup-ms)) nil))
  w)

(defn stop!
  "Ends the supervision and the image's process, and waits until both
   are gone. An evaluation that still runs in the image fails. Does
   nothing when the image was never started. Answers `w`."
  [{:rep/keys [image] :as w}]
  (let [{:image/keys [entry stop supervisor]} @image]
    (when stop
      (deliver stop true)
      (some-> (:image/starting @image) (session/end!))
      (when (realized? entry)
        (some-> (:session/process @entry) (session/end!)))
      (some-> supervisor (deref 20000 nil))))
  w)

(defn- obtain
  "The running image, or a `:session/error` with the reason why none
   runs. Waits up to `timeout-ms` for an image that is starting."
  [{:rep/keys [image]} timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [entry (:image/entry @image)
            remaining (- deadline (System/currentTimeMillis))
            value (some-> entry (deref (max 0 remaining) nil))]
        (cond
          (nil? entry)
          {:session/error "The image was not started. rep.server/start! and rep.image/start! start it."}

          (or (:session/error value) (session/alive? value))
          value

          (pos? remaining)
          (do
            (Thread/sleep 5)
            (recur))

          :else
          {:session/error (str "The image did not start within " timeout-ms " ms.")})))))

(defn- running-generation
  "The generation of the image when its process runs right now, else nil."
  [{:image/keys [entry]}]
  (let [value (when (some-> entry (realized?)) @entry)]
    (when (session/alive? value)
      (:image/generation value))))

(defn state
  "What the image is to `session`. :started the first time the uuid
   shows up. :continued when it evaluated in the image that runs now.
   :restarted when the image started again since its last evaluation, or
   when only the session's folder on disk is left (`known?`). In both
   cases its definitions are gone."
  [{:rep/keys [image]} session known?]
  (let [{:image/keys [seen] :as all} @image
        generation (get seen session)]
    (cond
      (and generation (= generation (running-generation all))) :continued
      (or generation known?) :restarted
      :else :started)))

(defn reserve!
  "Notes that `session` evaluates in the image that runs now, or in the
   next one when none runs."
  [{:rep/keys [image]} session]
  (swap! image
         (fn [{:image/keys [generation] :as all}]
           (assoc-in all
                     [:image/seen session]
                     (or (running-generation all) (inc generation)))))
  nil)

(defn process!
  "The running image, for an evaluation of `session`. Waits for an image
   that is starting. Check the answer for `:session/error`."
  [{:rep/keys [image] :as w} session]
  (let [{:image/keys [generation] :as value} (obtain w start-timeout-ms)]
    (when generation
      (swap! image assoc-in [:image/seen session] generation))
    value))

(defn status
  "What the image is right now:

     :image/state       :running, :starting, :failed when the last start
                        failed and the next try waits, or :stopped
     :image/generation  how many times the image started so far, so the
                        number of the one that runs
     :image/pid :image/port :image/start-ms
                        the process, its nREPL port and how long its
                        start took, while it runs
     :image/error :image/failures
                        why the last start failed, and how many failed
                        in a row"
  [{:rep/keys [image]}]
  (let [{:image/keys [entry generation stop]} @image
        value (when (some-> entry (realized?)) @entry)]
    (cond
      (or (nil? entry) (realized? stop))
      {:image/state :stopped :image/generation generation}

      (session/alive? value)
      (assoc (description value) :image/state :running)

      (:session/error value)
      {:image/state :failed
       :image/generation generation
       :image/error (:session/error value)
       :image/failures (:image/failures value)}

      :else
      {:image/state :starting :image/generation generation})))

(defn- borrow!
  "A connection to the image `running` with an nREPL session of its
   own. It is one that an earlier call gave back, else a new one."
  [{:image/keys [connections] :session/keys [port]}]
  (let [[idle _] (swap-vals! connections #(if (seq %) (pop %) %))]
    (or (peek idle)
        (let [connection (nrepl/connect port)]
          (try
            (assoc connection :nrepl/session (nrepl/clone! connection))
            (catch Throwable e
              (nrepl/close! connection)
              (throw e)))))))

(defn- discard!
  "Closes the nREPL session of `connection` and the connection itself."
  [{:nrepl/keys [socket session] :as connection}]
  (try
    (.setSoTimeout socket 5000)
    (nrepl/call! connection {"op" "close" "session" session})
    (catch Throwable _ nil)
    (finally
      (nrepl/close! connection))))

(defn- give-back!
  "Keeps `connection` for the next call. Discards it when enough are
   kept already, or when the image is gone."
  [{:image/keys [connections]} connection]
  (let [[before after] (swap-vals! connections
                                   #(if (and % (< (count %) idle-connections))
                                      (conj % connection)
                                      %))]
    (when (= (count before) (count after))
      (discard! connection))
    nil))

(defn- abandon!
  "Gives up on the eval `id`: asks the image to interrupt it, gives it a
   few seconds to say that the eval is done and discards the connection.
   All of that happens on a thread of its own, so that the caller has
   its answer at the deadline. A connection that closes while the image
   still writes to it has babashka print a stack trace."
  [{:nrepl/keys [socket session] :as connection} id]
  (future
    (try
      (nrepl/send! connection {"op" "interrupt"
                               "id" (str (random-uuid))
                               "session" session
                               "interrupt-id" id})
      (.setSoTimeout socket 5000)
      (loop []
        (let [message (nrepl/receive! connection)]
          (when-not (and (= id (:id message)) (nrepl/done? message))
            (recur))))
      (catch Throwable _ nil)
      (finally
        (discard! connection))))
  nil)

(defn- gone!
  "Waits a moment until the process of the image `running` is really
   gone. A connection breaks a little before the process counts as
   ended. A caller that hears \"the image's process ended\" and calls
   again at once would else be handed the same dying image and hear it
   a second time, instead of waiting for the next one."
  [running]
  (some-> (:proc (:session/process running))
          (.waitFor 2 java.util.concurrent.TimeUnit/SECONDS))
  nil)

(defn evaluate
  "Evaluates `code` in the image, for the host process itself: no HTTP,
   no evaluation id, nothing kept on disk. Blocks until the code is
   through and answers

     {:out \"...\" :value \"...\" :error \"...\"}

   `:out` is what the code printed, standard error included, in order.
   `:value` is the value of the last form as `pr-str` wrote it, \"nil\"
   for nil. `:error` is nil when the code ran through. When it threw,
   `:error` is babashka's error text and `:value` is nil. The code
   starts in the namespace `user` and stops at its first error, like the
   code of a POST.

   The option `:timeout-ms`, 10000 without it, limits the whole call,
   the wait for an image that is starting included. Past it the image is
   asked to interrupt the code, and `:error` says so. `:error` also
   tells when no image runs or when its process ended in the middle.

   Calls run side by side. Each one takes a connection that an earlier
   call left open, so one call is one message to the image and its
   answer. Throws without `:rep/shared-image?`."
  ([w code]
   (evaluate w code nil))
  ([w code {:keys [timeout-ms] :or {timeout-ms 10000}}]
   (shared! w "rep.image/evaluate")
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)
         running (obtain w timeout-ms)
         chunks (atom [])
         failure (fn [message]
                   {:out (:out (nrepl/outcome @chunks nil)) :value nil :error message})]
     (if (:session/error running)
       (failure (:session/error running))
       (try
         (let [connection (borrow! running)
               id (str (random-uuid))]
           (try
             (let [result (nrepl/converse! connection id (nrepl/wrapped code) deadline chunks)]
               (give-back! running connection)
               (nrepl/outcome @chunks result))
             (catch java.net.SocketTimeoutException _
               (abandon! connection id)
               (failure (str "The image did not answer within " timeout-ms " ms.")))
             (catch Throwable e
               (nrepl/close! connection)
               (throw e))))
         (catch java.io.IOException _
           (gone! running)
           (failure "The image's process ended.")))))))

(comment
  (require '[rep.server :as server])
  (def w (server/start! (server/prepare {:rep/dir "work"
                                         :rep/port 8730
                                         :rep/shared-image? true
                                         :rep/on-image-start (fn [_w image] (prn :started image))})))
  (status w)
  (evaluate w "(def counter (atom 0)) (swap! counter inc)")
  (evaluate w "(println \"hi\") (/ 1 0)")
  (evaluate w "(Thread/sleep 5000)" {:timeout-ms 200})

  ;; the process ends, the supervisor starts the next generation
  (evaluate w "(System/exit 0)")
  (status w)
  (state w "some-session-uuid" false)

  (pause-ms w 3)
  (server/stop! w))
