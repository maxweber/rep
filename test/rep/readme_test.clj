(ns rep.readme-test
  "The README's own commands, run against a real server. rep has no
   client: `curl` is the client, and the README is where a caller
   learns the commands. This test takes them out of the README's text
   and runs them as they stand, so that the text cannot drift from the
   routes. A command is a fenced block behind a line `<!-- test: name
   -->`."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rep.files-test :refer [megabyte random-file! sha256]]
            [rep.server :as server]
            [rep.server-test :as server-test :refer [with-system]]
            [rep.session :as session]))

(def readme
  (slurp (fs/file (fs/parent *file*) ".." ".." "README.md")))

(def commands
  "The commands of the README, by the name behind `test:`."
  (into {}
        (for [[_ name command] (re-seq #"(?s)<!-- test: ([a-z-]+) -->\n```[a-z]*\n(.*?)\n```" readme)]
          [name command])))

(def token
  "A token of its own for the caller of this test, so its session is
   no other test's."
  (server-test/sign {"sub" "readme"}))

(defn- run
  "Runs `script` with bash in the folder `dir`, as a caller that has
   the server's address and its token in the two variables the README
   names. Answers `{:exit :out :err}`."
  [w dir script]
  (-> (process/shell {:dir (str dir)
                      :out :string
                      :err :string
                      :continue true
                      :extra-env {"REP_URL" (server-test/address w)
                                  "REP_TOKEN" token
                                  "HOME" (str dir)}}
                     "bash" "-c" script)
      (select-keys [:exit :out :err])))

(defn- out
  "What the README's command `name` prints."
  [w dir name]
  (:out (run w dir (get commands name))))

(deftest marked-test
  (testing "every block of the README with a curl command in it is one this test runs"
    (let [blocks (map second (re-seq #"(?s)```[a-z]*\n(.*?)\n```" readme))
          with-curl (filter #(re-find #"\bcurl " %) blocks)]
      (is (<= 12 (count with-curl)))
      (is (empty? (remove (set (vals commands)) with-curl)))))
  (testing "and it knows every name"
    (is (= #{"evaluate" "define" "use" "error" "usage" "bash" "script" "future" "ask" "ids" "long"
             "put" "get" "parts-up" "parts-down" "babashka"}
           (set (keys commands))))))

(deftest evaluate-test
  (with-system {}
    (fn [w]
      (let [dir (fs/create-temp-dir {:prefix "rep-readme"})]
        (try
          (testing "evaluate"
            (is (= "3\n" (out w dir "evaluate"))))
          (testing "a string with quotes and a line break through a heredoc"
            (is (= "27\n" (out w dir "define"))))
          (testing "a definition that is still there in the next call"
            (is (= "it's \"quoted\",\non two lines\n" (out w dir "use"))))
          (testing "an error: babashka's text, and the outcome in the headers"
            (let [answer (out w dir "error")]
              (is (str/starts-with? answer "HTTP/1.1 200 OK\r\n"))
              (is (str/includes? answer "Rep-Status: failed\r\n"))
              (is (str/includes? answer "Rep-Exit: 1\r\n"))
              (is (str/ends-with? answer "\r\n\r\njava.lang.ArithmeticException: Divide by zero [at NO_SOURCE_PATH:1:1]\n"))))
          (testing "GET / needs no token and is the text for an agent"
            (is (= (server/usage w) (out w dir "usage"))))
          (testing "bash: its output as it wrote it, standard error too, and its exit code as a value"
            (is (re-matches #"(?s)\w+\nls: .*nothing-here.*\n\{:exit [12]\}\n" (out w dir "bash"))))
          (testing "a script that is awkward to quote goes up as a file"
            (spit (fs/file dir "cut.sh") "echo \"it's $((1+2)) \\\"quoted\\\"\"\nexit 4\n")
            (is (= (str "Stored scripts/cut.sh (" (fs/size (fs/file dir "cut.sh")) " bytes).\nit's 3 \"quoted\"\n{:exit 4}\n")
                   (out w dir "script"))))
          (testing "long work in a future, asked for in a later request"
            (spit (fs/file (:rep/dir w) "render.sh") "sleep 0.5\necho rendered\n")
            (fs/set-posix-file-permissions (fs/file (:rep/dir w) "render.sh") "rwxr-xr-x")
            (is (= "#'user/job\n" (out w dir "future")))
            (is (server-test/wait-until #(= "[true \"rendered\\n\"]\n" (out w dir "ask")))))
          (finally
            (fs/delete-tree dir)))))))

(deftest usage-commands-test
  (with-system {}
    (fn [w]
      (let [dir (fs/create-temp-dir {:prefix "rep-readme"})
            lines (str/split-lines (server/usage w))
            indented (->> (partition-by #(str/starts-with? % "  ") lines)
                          (filter #(str/starts-with? (first %) "  "))
                          (map (fn [block] (str/join "\n" (map #(subs % 2) block)))))
            [heredoc files] indented]
        (try
          (testing "GET / has three curl commands: the README runs two of them, word for word"
            (is (= 2 (count indented)))
            (is (= (str (get commands "put") "\n" (get commands "get")) files)))
          (testing "and the third runs here as it stands"
            (is (re-matches #"(?s)it's \"quoted\"\n.+\n\{:exit 0\}\n" (:out (run w dir heredoc)))))
          (finally
            (fs/delete-tree dir)))))))

(deftest dropped-connection-test
  (with-system {}
    (fn [w]
      (let [dir (fs/create-temp-dir {:prefix "rep-readme"})
            whole "step 1\nstep 2\nstep 3\nstep 4\n{:exit 0}\n"]
        (try
          (spit (fs/file dir "job.clj")
                (str "(spit \"runs.txt\" \"x\" :append true)\n"
                     "(sh \"for i in 1 2 3 4; do echo step $i; sleep 1; done\")\n"))
          (let [script (str/join "\n" [(get commands "ids")
                                       ;; the line drops after two seconds
                                       "curl() { command curl --max-time 2 \"$@\"; }"
                                       (get commands "long")
                                       "echo \"[cut $?]\""
                                       "unset -f curl"
                                       (get commands "long")
                                       "echo \"[again $?]\""
                                       (get commands "long")])
                [_ cut again after] (re-matches #"(?s)(.*)\[cut 28\]\n(.*)\[again 0\]\n(.*)" (:out (run w dir script)))]
            (testing "the connection drops in the middle of a long evaluation"
              (is (some? cut) "curl gave up with its exit code for a timeout")
              (is (re-matches #"(step \d\n){1,3}" (str cut)))
              (is (str/starts-with? whole (str cut))))
            (testing "the same command again, with the same id, follows it to the end from its first byte"
              (is (= whole again)))
            (testing "and once more after it ended: the stored answer"
              (is (= whole after)))
            (testing "the code ran once"
              (is (= "x" (slurp (fs/file (:rep/dir w) "runs.txt"))))))
          (finally
            (fs/delete-tree dir)))))))

(deftest files-test
  (let [small (random-file! (* 3 megabyte) 5)
        large (random-file! (* 20 megabyte) 6)]
    (try
      (with-system {:rep/max-body (* 9 megabyte)}
        (fn [w]
          (let [dir (fs/create-temp-dir {:prefix "rep-readme"})
                local (fs/file dir "clip.mp4")
                back (fs/file dir "final.mp4")
                stored (fs/file (:rep/dir w) "in" "clip.mp4")
                kept (fs/file (:rep/dir w) "out" "final.mp4")]
            (try
              (fs/create-dirs (fs/parent kept))
              (testing "a file up"
                (fs/copy small local)
                (is (= (str "Stored in/clip.mp4 (" (* 3 megabyte) " bytes).\n") (out w dir "put")))
                (is (= (sha256 small) (sha256 stored))))
              (testing "a file down"
                (fs/copy small kept)
                (is (= "" (out w dir "get")))
                (is (= (sha256 small) (sha256 back))))
              (testing "a file above one request's size does not go up in one PUT"
                (fs/copy large local {:replace-existing true})
                (is (not (str/starts-with? (out w dir "put") "Stored")))
                (is (= (sha256 small) (sha256 stored))))
              (testing "it goes up in parts, with the loop of the README"
                (is (= {:exit 0 :out "" :err ""} (run w dir (get commands "parts-up"))))
                (is (= (* 20 megabyte) (fs/size stored)))
                (is (= (sha256 large) (sha256 stored))))
              (testing "and comes down in parts, with the other loop"
                (fs/copy large kept {:replace-existing true})
                (is (= {:exit 0 :out "" :err ""} (run w dir (get commands "parts-down"))))
                (is (= (sha256 large) (sha256 back))))
              (testing "the loop stops at a part the server refuses, here for a token that is none"
                (let [refused (-> (process/shell {:dir (str dir) :out :string :err :string :continue true
                                                  :extra-env {"REP_URL" (server-test/address w)
                                                              "REP_TOKEN" "wrong"
                                                              "HOME" (str dir)}}
                                                 "bash" "-c" (str (get commands "parts-up") "\necho \"[$OFFSET]\""))
                                  :out)]
                  (is (= "[]\n" refused))
                  (is (= (sha256 large) (sha256 stored)) "and the file is as it was")))
              (finally
                (fs/delete-tree dir))))))
      (finally
        (fs/delete-if-exists small)
        (fs/delete-if-exists large)))))

(deftest babashka-test
  (with-system {}
    (fn [w]
      (let [dir (fs/create-temp-dir {:prefix "rep-readme"})
            program (get commands "babashka")]
        (try
          (spit (fs/file dir "caller.clj") program)
          (testing "the caller in babashka of the README answers what its comment says"
            (let [{:keys [exit out]} (run w dir (str (pr-str (session/babashka))
                                                     " -e '(prn (load-file \"caller.clj\"))'"))]
              (is (= 0 exit))
              (is (= {:text "3\n" :status "done"} (edn/read-string out)))
              (is (= (edn/read-string out)
                     (edn/read-string (second (re-find #";; => (.*)" program)))))))
          (finally
            (fs/delete-tree dir)))))))
