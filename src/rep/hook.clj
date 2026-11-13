(ns rep.hook
  "The functions a host puts into the options to hear about what
   happens: `:rep/on-image-start` and `:rep/after-evaluation`. They run
   in the server's process. A hook that throws takes nothing down with
   it, neither a request nor the supervision of the image.")

(defn call!
  "Calls the hook that the system `w` has under `k` with `w` and
   `argument`. Prints what the hook throws to standard error. Answers
   nil, with or without a hook."
  [w k argument]
  (when-let [hook (get w k)]
    (try
      (hook w argument)
      (catch Throwable e
        (binding [*out* *err*]
          (println (str "rep: the hook " k " threw: "
                        (or (ex-message e) (.getName (class e)))))))))
  nil)

(comment
  (call! {:rep/after-evaluation (fn [_w info] (prn info))}
         :rep/after-evaluation
         {:evaluation/state :done})
  (call! {:rep/after-evaluation (fn [_w _info] (throw (ex-info "broken" {})))}
         :rep/after-evaluation
         {})
  (call! {} :rep/after-evaluation {}))
