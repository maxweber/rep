(ns rep.admin
  "What the person who runs the server does from a terminal: make a login
   code, list the tokens, end one. Each works on the state folder, so it
   works while the server runs."
  (:require [babashka.cli :as cli]
            [rep.auth :as auth]
            [rep.server :as server]))

(def cli-spec
  {:dir {:desc "Work folder of the server (REP_DIR, else work)"}})

(defn- system
  "The system map for the work folder the options name."
  [{:keys [dir]}]
  (server/prepare (cond-> {}
                    dir (assoc :rep/dir dir))))

(defn- date
  "The second `seconds` as a time in UTC."
  [seconds]
  (str (java.time.Instant/ofEpochSecond seconds)))

(defn code
  "bb code [--dir work]: prints a new login code."
  [& args]
  (let [w (system (cli/parse-opts args {:spec cli-spec}))]
    (println (auth/mint-code! w))
    (binding [*out* *err*]
      (println (str "Good once and for " (:rep/code-minutes w) " minutes.")))))

(defn tokens
  "bb tokens [--dir work]: lists the tokens that are still good."
  [& args]
  (let [records (auth/tokens (system (cli/parse-opts args {:spec cli-spec})))]
    (if (empty? records)
      (println "No tokens.")
      (doseq [{:keys [jti issued expires]} records]
        (println jti "issued" (date issued) "expires" (date expires))))))

(defn revoke
  "bb revoke <jti> [--dir work]: ends a token."
  [& args]
  (let [{:keys [opts args]} (cli/parse-args args {:spec cli-spec})
        jti (first args)]
    (if (and jti (auth/revoke! (system opts) jti))
      (println "Revoked" jti)
      (do
        (binding [*out* *err*]
          (println "No such token. bb tokens lists them."))
        (System/exit 1)))))
