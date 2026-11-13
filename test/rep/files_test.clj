(ns rep.files-test
  "Files that are larger than one request: a body the server refuses,
   an upload and a download in parts, and the offsets of a part."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [rep.client :as client]
            [rep.files :as files]
            [rep.server :as server]
            [rep.server-test :as server-test :refer [evaluate uuid with-system]]))

(def megabyte
  (* 1024 1024))

(defn sha256
  "The SHA-256 of the file in hex, read in pieces."
  [file]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        buffer (byte-array 65536)]
    (with-open [in (java.io.FileInputStream. (str file))]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn random-file!
  "Writes `bytes` random bytes into a new file and answers its path. The
   same `seed` gives the same bytes."
  [bytes seed]
  (let [file (str (fs/create-temp-file {:prefix "rep-file" :suffix ".bin"}))
        random (java.util.Random. seed)
        buffer (byte-array megabyte)]
    (with-open [out (java.io.FileOutputStream. file)]
      (loop [left bytes]
        (when (pos? left)
          (.nextBytes random buffer)
          (.write out buffer 0 (int (min left megabyte)))
          (recur (- left megabyte)))))
    file))

(defn- as-client
  "The system as `rep.client` sees it, with a session of its own."
  [w]
  {:client/url (server-test/address w)
   :client/token server-test/token
   :client/session (uuid)})

(defn- upload-url
  "A signed URL that takes `path`, asked for the way a caller does."
  [w path]
  (edn/read-string (:body (evaluate w (uuid) (str "(upload-url " (pr-str path) ")")))))

(defn- put
  "PUTs `body` to the signed URL, as the part at `offset` when given."
  ([url body]
   (http/put url {:body body :throw false}))
  ([url body offset]
   (http/put (str url "&offset=" offset) {:body body :throw false})))

(defn- size-header
  [response]
  (get (:headers response) (str/lower-case files/size-header)))

(deftest max-body-test
  (with-system {:rep/max-body (* 64 1024)}
    (fn [w]
      (let [session (uuid)
            id (uuid)
            code (str "(spit \"ran.txt\" \"x\")" (apply str (repeat (* 70 1024) " ")))
            answer (try
                     (server-test/post w session code {:id id})
                     (catch java.io.IOException e e))]
        (testing "a body above :rep/max-body is refused"
          (is (or (instance? java.io.IOException answer)
                  (= 413 (:status answer)))
              "with a 413, or the server hangs up while the body is still on its way"))
        (testing "and the handler never ran: the evaluation has no folder, its code did nothing"
          (is (not (fs/exists? (fs/path (:rep/state-dir w) "sessions" session))))
          (is (not (fs/exists? (fs/file (:rep/dir w) "ran.txt")))))
        (testing "a body under the limit is taken as before"
          (is (= "3\n" (:body (evaluate w session "(+ 1 2)")))))))))

(deftest default-max-body-test
  (with-system {}
    (fn [w]
      (is (= (* 32 megabyte) server/max-body (:rep/max-body w)))))
  (is (= {:rep/max-body (* 5 megabyte)} (server/cli-options ["--max-body-mb" "5"]))))

(deftest parts-test
  (let [local (random-file! (* 20 megabyte) 42)
        back (str local ".back")]
    (try
      (with-system {:rep/max-body (* 9 megabyte)}
        (fn [w]
          (let [stored (fs/file (:rep/dir w) "in" "big.bin")]
            (testing "one request cannot carry the file"
              (let [answer (try
                             (put (upload-url w "in/whole.bin") (fs/file local))
                             (catch java.io.IOException e e))]
                (is (or (instance? java.io.IOException answer) (= 413 (:status answer))))
                (is (not (fs/exists? (fs/file (:rep/dir w) "in" "whole.bin"))))))
            (testing "20 MB go up in parts through one signed URL and arrive intact"
              (is (= (* 20 megabyte) (client/upload! (as-client w) local "in/big.bin")))
              (is (= (* 20 megabyte) (fs/size stored)))
              (is (= (sha256 local) (sha256 stored))))
            (testing "and come back in parts"
              (is (= (* 20 megabyte) (client/download! (as-client w) "in/big.bin" back)))
              (is (= (sha256 local) (sha256 back))))
            (testing "the same upload again starts the file over, it does not append"
              (is (= (* 20 megabyte) (client/upload! (as-client w) local "in/big.bin")))
              (is (= (sha256 local) (sha256 stored)))))))
      (finally
        (fs/delete-if-exists local)
        (fs/delete-if-exists back)))))

(deftest offset-test
  (with-system {}
    (fn [w]
      (let [url (upload-url w "in/parts.txt")
            stored (fs/file (:rep/dir w) "in" "parts.txt")]
        (testing "offset 0 starts the file, the answer says how many bytes it holds"
          (let [answer (put url "hello " 0)]
            (is (= 201 (:status answer)))
            (is (= "6" (size-header answer)))
            (is (= "Stored in/parts.txt (6 bytes).\n" (:body answer)))))
        (testing "the file's size as the offset appends"
          (let [answer (put url "world" 6)]
            (is (= 201 (:status answer)))
            (is (= "11" (size-header answer)))
            (is (= "hello world" (slurp stored)))))
        (testing "any other offset is a 409 that says the size and changes nothing"
          (doseq [offset [5 12 100]]
            (let [answer (put url "!" offset)]
              (is (= 409 (:status answer)) (str "offset " offset))
              (is (= "11" (size-header answer)))
              (is (str/includes? (:body answer) "holds 11 bytes"))))
          (is (= "hello world" (slurp stored))))
        (testing "an offset that is no number of bytes is a 400"
          (is (= 400 (:status (put url "!" "x"))))
          (is (= 400 (:status (put url "!" -1))))
          (is (= "hello world" (slurp stored))))
        (testing "offset 0 again starts the file over"
          (is (= "3" (size-header (put url "new" 0))))
          (is (= "new" (slurp stored))))
        (testing "a PUT without an offset is the whole file, as before"
          (let [answer (put url "the whole file")]
            (is (= 201 (:status answer)))
            (is (= "14" (size-header answer)))
            (is (= "the whole file" (slurp stored)))))
        (testing "an empty part at offset 0 makes an empty file"
          (is (= "0" (size-header (put url "" 0))))
          (is (= "" (slurp stored))))
        (testing "the offset does not get a part past the signature"
          (is (= 403 (:status (put (str/replace url "parts.txt" "other.txt") "x" 0)))))))))

(deftest range-test
  (with-system {}
    (fn [w]
      (spit (fs/file (:rep/dir w) "letters.txt") "abcdefghij")
      (let [url (edn/read-string (:body (evaluate w (uuid) "(download-url \"letters.txt\")")))
            ask (fn [range]
                  (http/get url {:headers {"Range" range} :throw false}))]
        (testing "a range answers 206 with that part and the size of the file"
          (let [answer (ask "bytes=2-5")]
            (is (= 206 (:status answer)))
            (is (= "cdef" (:body answer)))
            (is (= "bytes 2-5/10" (get (:headers answer) "content-range")))))
        (testing "a range without an end, and one that ends past the file"
          (is (= "hij" (:body (ask "bytes=7-"))))
          (is (= "bytes 7-9/10" (get (:headers (ask "bytes=7-99")) "content-range"))))
        (testing "a range that starts past the end is a 416 that says the size"
          (let [answer (ask "bytes=10-")]
            (is (= 416 (:status answer)))
            (is (= "bytes */10" (get (:headers answer) "content-range")))))
        (testing "so is every range that is malformed or names no byte, a number too large for a long too"
          (doseq [range ["bytes=99999999999999999999-" "bytes=99999999999999999999-99999999999999999999"
                         "bytes=-5" "bytes=9-3" "bytes=a-b" "items=0-1" "bytes=0-1,4-5" ""]]
            (let [answer (ask range)]
              (is (= 416 (:status answer)) (pr-str range))
              (is (= "bytes */10" (get (:headers answer) "content-range")) (pr-str range)))))
        (testing "an end that is too large a number counts as the end of the file"
          (is (= "ij" (:body (ask "bytes=8-99999999999999999999")))))
        (testing "without the header the whole file comes, as before"
          (let [answer (http/get url {:throw false})]
            (is (= 200 (:status answer)))
            (is (= "abcdefghij" (:body answer)))))))))

(deftest byte-range-test
  (is (= [0 99] (files/byte-range "bytes=0-99" 1000)))
  (is (= [900 999] (files/byte-range "bytes=900-" 1000)))
  (testing "one answer carries at most range-bytes"
    (is (= [0 (dec files/range-bytes)] (files/byte-range "bytes=0-" (* 3 files/range-bytes)))))
  (testing "no header is no range"
    (is (nil? (files/byte-range nil 1000))))
  (testing "a header of another shape, and a range that names no byte of the file, cannot be answered"
    (doseq [header ["bytes=1000-" "bytes=-5" "bytes=9-3" "bytes=99999999999999999999-" "lines=1-2" ""]]
      (is (= :unsatisfiable (files/byte-range header 1000)) (pr-str header)))
    (is (= :unsatisfiable (files/byte-range "bytes=0-" 0)) "an empty file has no byte")))

(deftest max-file-test
  (with-system {:rep/max-file-mb 1}
    (fn [w]
      (let [url (upload-url w "in/grows.bin")
            stored (fs/file (:rep/dir w) "in" "grows.bin")
            part (byte-array (* 700 1024))]
        (testing "a file grows through its parts up to :rep/max-file-mb"
          (is (= 201 (:status (put url part 0)))))
        (testing "a part that would pass the limit is a 413 with a sentence, and changes nothing"
          (let [answer (put url part (* 700 1024))]
            (is (= 413 (:status answer)))
            (is (= "in/grows.bin would grow past 1 MB, the largest file this server takes in parts (:rep/max-file-mb).\n"
                   (:body answer))))
          (is (= (* 700 1024) (fs/size stored))))
        (testing "a part that still fits is taken"
          (is (= 201 (:status (put url (byte-array (* 324 1024)) (* 700 1024)))))
          (is (= megabyte (fs/size stored))))
        (testing "a first part that is too large alone does not start the file over"
          (is (= 413 (:status (put url (byte-array (+ megabyte 1)) 0))))
          (is (= megabyte (fs/size stored)))))))
  (testing "the limit is 4096 MB when nobody says otherwise"
    (with-system {}
      (fn [w]
        (is (= 4096 files/max-file-mb (:rep/max-file-mb w)))))))

(defn- put-as
  "PUTs `body` as the part at `offset` of the upload `id`. `more` goes
   on the end of the query."
  ([url body offset id]
   (put-as url body offset id nil))
  ([url body offset id more]
   (http/put (str url "&offset=" offset "&upload=" id more) {:body body :throw false})))

(deftest upload-id-test
  (with-system {}
    (fn [w]
      (let [url (upload-url w "in/shared.txt")
            stored (fs/file (:rep/dir w) "in" "shared.txt")]
        (testing "the part at offset 0 puts its upload on record, and the upload goes on"
          (is (= 201 (:status (put-as url "aaa" 0 "upload-a"))))
          (is (= 201 (:status (put-as url "aaa" 3 "upload-a"))))
          (is (= "aaaaaa" (slurp stored))))
        (testing "a part of another upload is a 409 that says so and changes nothing, even at the right offset"
          (let [answer (put-as url "bbb" 6 "upload-b")]
            (is (= 409 (:status answer)))
            (is (str/starts-with? (:body answer) "Another upload writes in/shared.txt."))
            (is (nil? (size-header answer)) "it is not the 409 of a wrong offset"))
          (is (= "aaaaaa" (slurp stored))))
        (testing "the upload on record goes on as if nothing happened"
          (is (= "9" (size-header (put-as url "aaa" 6 "upload-a")))))
        (testing "with resume=true another upload takes the file over, and the first one is told"
          (is (= 201 (:status (put-as url "bbb" 9 "upload-b" "&resume=true"))))
          (is (= 409 (:status (put-as url "aaa" 12 "upload-a"))))
          (is (= 201 (:status (put-as url "bbb" 12 "upload-b"))))
          (is (= "aaaaaaaaabbbbbb" (slurp stored))))
        (testing "a wrong offset of the right upload is still the 409 that says the size"
          (is (= "15" (size-header (put-as url "bbb" 99 "upload-b")))))
        (testing "a part without an id is taken as before"
          (is (= 201 (:status (put url "ccc" 15))))
          (is (= "aaaaaaaaabbbbbbccc" (slurp stored))))
        (testing "offset 0 starts the file over for whoever sends it"
          (is (= 201 (:status (put-as url "ddd" 0 "upload-d"))))
          (is (= 409 (:status (put-as url "bbb" 3 "upload-b"))))
          (is (= "ddd" (slurp stored))))
        (testing "a first part without an id clears the record, any upload may go on then"
          (is (= 201 (:status (put url "eee" 0))))
          (is (= 201 (:status (put-as url "fff" 3 "upload-f"))))
          (is (= 409 (:status (put-as url "ddd" 6 "upload-d"))))
          (is (= "eeefff" (slurp stored))))
        (testing "the record is beside rep's own files, not among the work folder's"
          (is (= ["shared.txt"] (map fs/file-name (fs/list-dir (fs/parent stored)))))
          (is (= 1 (count (fs/list-dir (fs/path (:rep/state-dir w) "uploads"))))))
        (testing "records of uploads that began more than :rep/keep-days ago are deleted"
          (let [now (System/currentTimeMillis)
                day (* 24 60 60 1000)]
            (is (= 0 (files/forget-uploads! (assoc w :rep/now-ms (+ now (* 6 day))))))
            (is (= 0 (files/forget-uploads! (assoc w :rep/now-ms (+ now (* 400 day)) :rep/keep-days 0))))
            (is (= 1 (files/forget-uploads! (assoc w :rep/now-ms (+ now (* 8 day))))))
            (is (= 201 (:status (put-as url "ggg" 6 "upload-g"))) "an upload that still ran goes on under its id")))))))

(defn- slowly
  "A stand-in for a function of the HTTP client that waits `ms` before
   every call, as a slow line does, and notes the status of every
   answer in the atom `seen`."
  [request ms seen]
  (fn [url options]
    (Thread/sleep ms)
    (let [answer (request url options)]
      (swap! seen conj (:status answer))
      answer)))

(defn- cut-after
  "A stand-in for a function of the HTTP client that lets `n` calls
   through and breaks the connection of every later one."
  [request n]
  (let [calls (atom 0)]
    (fn [url options]
      (if (< n (swap! calls inc))
        (throw (java.io.IOException. "The line is cut."))
        (request url options)))))

(deftest url-lifetime-test
  (let [local (random-file! (* 2 megabyte) 15)
        back (str local ".back")
        put http/put
        get http/get]
    (try
      (with-system {:rep/url-seconds 1}
        (fn [w]
          (let [stored (fs/file (:rep/dir w) "in" "slow.bin")]
            (testing "an upload that takes longer than a signed URL holds asks for a fresh one and completes"
              (let [seen (atom [])]
                (with-redefs [client/part-bytes (* 256 1024)
                              http/put (slowly put 350 seen)]
                  (is (= (* 2 megabyte) (client/upload! (as-client w) local "in/slow.bin"))))
                (is (some #{403} @seen) "the server refused a URL that had run out in the middle")
                (is (= 8 (count (filter #{201} @seen))) "and no part went twice")
                (is (= (sha256 local) (sha256 stored)))))
            (testing "so does a download"
              (let [seen (atom [])]
                (with-redefs [client/part-bytes (* 256 1024)
                              http/get (slowly get 350 seen)]
                  (is (= (* 2 megabyte) (client/download! (as-client w) "in/slow.bin" back))))
                (is (some #{403} @seen))
                (is (= 8 (count (filter #{206} @seen))))
                (is (= (sha256 local) (sha256 back))))))))
      (finally
        (fs/delete-if-exists local)
        (fs/delete-if-exists back)))))

(deftest resume-test
  (let [local (random-file! (* 2 megabyte) 16)
        back (str local ".back")
        part (str back ".part")
        put http/put
        get http/get]
    (try
      (with-system {}
        (fn [w]
          (let [stored (fs/file (:rep/dir w) "in" "cut.bin")]
            (testing "an upload that was cut leaves the parts that arrived"
              (with-redefs [client/part-bytes (* 256 1024)
                            client/part-tries 1
                            http/put (cut-after put 3)]
                (is (thrown-with-msg? clojure.lang.ExceptionInfo #"The server does not answer"
                                      (client/upload! (as-client w) local "in/cut.bin"))))
              (is (= (* 3 256 1024) (fs/size stored))))
            (testing "another upload of the path is told that one writes the file already"
              (is (= 409 (:status (put-as (upload-url w "in/cut.bin") "x" (* 3 256 1024) "another")))))
            (testing "with :resume? it asks how much the server has and sends the rest only"
              (let [seen (atom [])]
                (with-redefs [client/part-bytes (* 256 1024)
                              http/put (slowly put 0 seen)]
                  (is (= (* 2 megabyte) (client/upload! (as-client w) local "in/cut.bin" {:resume? true}))))
                (is (= (repeat 5 201) @seen) "five parts were left of eight"))
              (is (= (sha256 local) (sha256 stored))))
            (testing "a resume of a file that is all there sends nothing"
              (with-redefs [http/put (fn [& _] (throw (ex-info "Nothing should be sent." {})))]
                (is (= (* 2 megabyte) (client/upload! (as-client w) local "in/cut.bin" {:resume? true})))))
            (testing "a resume refuses a file that is longer on the server than here"
              (spit stored "x" :append true)
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"That is another file"
                                    (client/upload! (as-client w) local "in/cut.bin" {:resume? true})))
              (client/upload! (as-client w) local "in/cut.bin"))
            (testing "a download that was cut leaves the part file with what came, and no half file"
              (with-redefs [client/part-bytes (* 256 1024)
                            client/part-tries 1
                            http/get (cut-after get 3)]
                (is (thrown-with-msg? clojure.lang.ExceptionInfo #"The server does not answer"
                                      (client/download! (as-client w) "in/cut.bin" back))))
              (is (= (* 3 256 1024) (fs/size part)))
              (is (not (fs/exists? back))))
            (testing "with :resume? it goes on from the part file's size"
              (let [seen (atom [])]
                (with-redefs [client/part-bytes (* 256 1024)
                              http/get (slowly get 0 seen)]
                  (is (= (* 2 megabyte) (client/download! (as-client w) "in/cut.bin" back {:resume? true}))))
                (is (= (repeat 5 206) @seen)))
              (is (= (sha256 local) (sha256 back)))
              (is (not (fs/exists? part))))
            (testing "without a part file a resume takes the local file for what came so far"
              (with-open [out (java.io.FileOutputStream. back)]
                (.write out (fs/read-all-bytes local) 0 megabyte))
              (is (= (* 2 megabyte) (client/download! (as-client w) "in/cut.bin" back {:resume? true})))
              (is (= (sha256 local) (sha256 back))))
            (testing "a download without :resume? starts over"
              (spit part "left over from another file")
              (is (= (* 2 megabyte) (client/download! (as-client w) "in/cut.bin" back)))
              (is (= (sha256 local) (sha256 back)))))))
      (finally
        (run! fs/delete-if-exists [local back part])))))

(deftest command-test
  (let [local (random-file! (* 3 megabyte) 7)
        back (str local ".back")
        script (str (fs/absolutize (fs/path "bin" "rep")))]
    (try
      (with-system {}
        (fn [w]
          (let [rep (fn [& args]
                      (-> (apply process/shell
                                 {:in ""
                                  :out :string
                                  :err :string
                                  :continue true
                                  :dir (:rep/dir w)
                                  :extra-env {"REP_URL" (server-test/address w)
                                              "REP_TOKEN" server-test/token}}
                                 script args)
                          (select-keys [:exit :out :err])))]
            (testing "rep upload LOCAL PATH"
              (is (= {:exit 0 :out (str "Stored in/clip.bin (" (* 3 megabyte) " bytes).\n") :err ""}
                     (rep "upload" local "in/clip.bin")))
              (is (= (sha256 local) (sha256 (fs/file (:rep/dir w) "in" "clip.bin")))))
            (testing "rep download PATH LOCAL"
              (is (= {:exit 0 :out (str "Fetched in/clip.bin into " back " (" (* 3 megabyte) " bytes).\n") :err ""}
                     (rep "download" "in/clip.bin" back)))
              (is (= (sha256 local) (sha256 back))))
            (testing "what does not work ends with 1 and a sentence"
              (let [{:keys [exit err]} (rep "download" "in/nothing.bin" back)]
                (is (= 1 exit))
                (is (str/includes? err "No file in/nothing.bin")))
              (let [{:keys [exit err]} (rep "upload" (str local ".missing") "in/x.bin")]
                (is (= 1 exit))
                (is (str/includes? err "There is no file")))
              (let [{:keys [exit err]} (rep "upload" local "../outside.bin")]
                (is (= 1 exit))
                (is (str/includes? err "The session gave no URL"))))
            (testing "a command without its two arguments says how it goes"
              (is (= 2 (:exit (rep "upload" local)))))
            (testing "rep upload --resume goes on with a file of which the server has the beginning"
              (let [stored (fs/file (:rep/dir w) "in" "resumed.bin")]
                (fs/create-dirs (fs/parent stored))
                (with-open [out (java.io.FileOutputStream. stored)]
                  (.write out (fs/read-all-bytes local) 0 megabyte))
                (is (= {:exit 0 :out (str "Stored in/resumed.bin (" (* 3 megabyte) " bytes).\n") :err ""}
                       (rep "upload" "--resume" local "in/resumed.bin")))
                (is (= (sha256 local) (sha256 stored)))))
            (testing "rep download --resume goes on with a local file that has the beginning"
              (with-open [out (java.io.FileOutputStream. back)]
                (.write out (fs/read-all-bytes local) 0 megabyte))
              (is (= 0 (:exit (rep "download" "--resume" "in/clip.bin" back))))
              (is (= (sha256 local) (sha256 back))))
            (testing "a download that fails at its first byte leaves no part file"
              (rep "download" "in/nothing.bin" back)
              (is (not (fs/exists? (str back ".part")))))
            (testing "an empty file goes both ways"
              (spit local "")
              (is (= 0 (:exit (rep "upload" local "in/empty.bin"))))
              (is (= 0 (fs/size (fs/file (:rep/dir w) "in" "empty.bin"))))
              (is (= 0 (:exit (rep "download" "in/empty.bin" back))))
              (is (= 0 (fs/size back)))))))
      (finally
        (fs/delete-if-exists local)
        (fs/delete-if-exists back)))))
