(ns rep.client-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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
          (is (= "Error: Divide by zero\n" out)))))))

(deftest bash-test
  (server-test/with-system {}
    (fn [w]
      (testing "a script goes in and its output comes out without any quoting"
        (is (= {:exit 3 :out "it's \"3\"\n" :err ""}
               (rep w "echo \"it's \\\"$((1+2))\\\"\"\nexit 3\n" "--sh")))))))

(deftest wait-attach-stop-test
  (server-test/with-system {}
    (fn [w]
      (let [id (server-test/uuid)
            waited (rep w "echo one; sleep 4; echo two" "--sh" "--id" id "--wait" "1")]
        (testing "the client lets go after the wait, the evaluation goes on"
          (is (= 124 (:exit waited)))
          (is (= "one\n" (:out waited)))
          (is (str/includes? (:err waited) (str "rep attach " id))))
        (testing "attach prints it from the start and follows to the end"
          (is (= {:exit 0 :out "one\ntwo\n" :err ""} (rep w "" "attach" id))))
        (testing "the same id again answers the same text and says so"
          (let [again (rep w "echo one; sleep 4; echo two" "--sh" "--id" id)]
            (is (= "one\ntwo\n" (:out again)))
            (is (str/includes? (:err again) "nothing ran again")))))
      (testing "stop"
        (let [id (server-test/uuid)
              running (future (rep w "echo begun; sleep 30" "--sh" "--id" id))]
          (Thread/sleep 500)
          (is (= 0 (:exit (rep w "" "stop" id))))
          (let [{:keys [exit out]} (deref running 5000 nil)]
            (is (= 130 exit))
            (is (= "begun\n" out))))))))

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
