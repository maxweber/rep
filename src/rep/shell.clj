(ns rep.shell
  "Runs a bash script and hands its output on as the bytes bash wrote,
   standard error included, in the order they came. Nothing is quoted or
   escaped on the way."
  (:require [babashka.process :as process]))

(def hidden-variables
  "What the scripts do not inherit from the server's environment."
  #{"REP_TOKEN" "REP_SECRET"})

(defn environment
  "The server's environment without the hidden variables."
  []
  (apply dissoc (into {} (System/getenv)) hidden-variables))

(defn bash!
  "Runs `:shell/script` with bash in `:shell/dir` and adds `:shell/exit`.
   Calls `:shell/write!` with `bytes offset length` for every chunk of
   output and `:shell/started!` once with the process, so that somebody
   else can end it. The script gets no input."
  [{:shell/keys [script dir write! started!] :as w}]
  (let [running (process/process {:cmd ["bash" "-c" script]
                                  :dir (str dir)
                                  :err :out
                                  :env (environment)})
        in (:out running)
        buffer (byte-array 8192)]
    (.close (:in running))
    (when started!
      (started! running))
    (try
      (loop []
        (let [n (.read in buffer)]
          (when-not (neg? n)
            (write! buffer 0 n)
            (recur))))
      (assoc w :shell/exit (:exit @running))
      (catch Throwable e
        (process/destroy-tree running)
        (throw e)))))

(comment
  (bash! {:shell/script "echo one; echo two >&2; exit 3"
          :shell/dir "."
          :shell/write! (fn [data offset length]
                          (print (String. data offset length "UTF-8")))}))
