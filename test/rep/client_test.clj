(ns rep.client-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rep.auth :as auth]
            [rep.server-test :as server-test]))

(def script
  (str (fs/absolutize (fs/path "bin" "rep"))))

(defn rep
  "Runs the client in the system's work folder with `input` on standard
   input. Answers `{:exit :out :err}`."
  [w input & args]
  (-> (apply process/shell
             {:in input
              :out :string
              :err :string
              :continue true
              :dir (:rep/dir w)
              :extra-env {"REP_URL" (server-test/address w)
                          "REP_TOKEN" server-test/token}}
             script args)
      (select-keys [:exit :out :err])))

(deftest evaluate-test
  (server-test/with-system {}
    (fn [w]
      (is (= {:exit 0 :out "3\n" :err ""} (rep w "(+ 1 2)")))
      (testing "the session id is kept in a file, so definitions stay"
        (is (= 0 (:exit (rep w "(def a 20)"))))
        (is (= "40\n" (:out (rep w "(* 2 a)"))))
        (is (= (str/trim (slurp (fs/file (:rep/dir w) ".rep-session")))
               (str/trim (:out (rep w "" "session"))))))
      (testing "an error ends with exit code 1"
        (let [{:keys [exit out]} (rep w "(/ 1 0)")]
          (is (= 1 exit))
          (is (str/includes? out "Divide by zero")))))))

(deftest bash-test
  (server-test/with-system {}
    (fn [w]
      (testing "the output of bash comes out as bash wrote it, its exit code is a value"
        (is (= {:exit 0 :out "it's \"3\"\n{:exit 3}\n" :err ""}
               (rep w (server-test/bash "echo \"it's \\\"$((1+2))\\\"\"\nexit 3\n"))))))))

(deftest wait-test
  (server-test/with-system {}
    (fn [w]
      (let [id (server-test/uuid)
            code (server-test/bash "echo one; sleep 5; echo two")
            waited (rep w code "--id" id "--wait" "2")]
        (testing "the client lets go after the wait, the evaluation goes on"
          (is (= 124 (:exit waited)))
          (is (= "one\n" (:out waited)))
          (is (str/includes? (:err waited) (str "--id " id))))
        (testing "the same input with the same id follows it to the end"
          (let [again (rep w code "--id" id)]
            (is (= 0 (:exit again)))
            (is (= "one\ntwo\n{:exit 0}\n" (:out again)))
            (is (str/includes? (:err again) "nothing ran again"))))))))

(deftest refusal-test
  (server-test/with-system {}
    (fn [w]
      (let [{:keys [exit err]} (-> (process/shell {:in "(+ 1 2)"
                                                   :out :string
                                                   :err :string
                                                   :continue true
                                                   :dir (:rep/dir w)
                                                   :extra-env {"REP_URL" (server-test/address w)
                                                               "REP_TOKEN" "wrong"}}
                                                  script)
                                   (select-keys [:exit :err]))]
        (is (= 2 exit))
        (is (str/includes? err "Authorization: Bearer"))))))

(defn rep-with
  "Runs the client like `rep`, with `env` and without a `REP_TOKEN`."
  [w env input & args]
  (-> (apply process/shell
             {:in input
              :out :string
              :err :string
              :continue true
              :dir (:rep/dir w)
              :extra-env (merge {"REP_URL" (server-test/address w)} env)}
             script args)
      (select-keys [:exit :out :err])))

(deftest login-test
  (let [config (str (fs/create-temp-dir {:prefix "rep-config"}))
        env {"XDG_CONFIG_HOME" config}]
    (try
      (server-test/with-system {:rep/token nil}
        (fn [w]
          (let [file (fs/path config "rep" (str "127.0.0.1-" (:rep/port w) ".token"))]
            (testing "without a token the client says how to get one"
              (let [{:keys [exit err]} (rep-with w env "(+ 1 2)")]
                (is (= 2 exit))
                (is (str/includes? err "rep login"))))
            (testing "a wrong code"
              (is (= 2 (:exit (rep-with w env "" "login" "AAAA-BBBB-CCCC"))))
              (is (not (fs/exists? file))))
            (testing "login stores the token for this server and does not print it"
              (let [{:keys [exit out]} (rep-with w env "" "login" (auth/mint-code! w))
                    token (slurp (fs/file file))]
                (is (= 0 exit))
                (is (re-find #"^Logged in to http://127.0.0.1:\d+ until \d{4}-\d{2}-\d{2}\.\n$" out))
                (is (not (str/includes? out token)))
                (is (= "rw-------" (fs/posix->str (fs/posix-file-permissions file))))
                (is (empty? (filter #(str/includes? (fs/file-name %) "token")
                                    (fs/list-dir (:rep/dir w))))
                    "nothing is left in the current folder")))
            (testing "the next call needs no REP_TOKEN"
              (is (= {:exit 0 :out "3\n" :err ""} (rep-with w env "(+ 1 2)"))))
            (testing "logout ends the token and removes the file"
              (let [token (slurp (fs/file file))]
                (is (= {:exit 0 :out "Logged out.\n" :err ""} (rep-with w env "" "logout")))
                (is (not (fs/exists? file)))
                (is (= 401 (:status (server-test/with-bearer w token "1"))))
                (is (= 2 (:exit (rep-with w env "(+ 1 2)")))))))))
      (finally
        (fs/delete-tree config)))))
