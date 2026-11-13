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
        (testing "without the header the whole file comes, as before"
          (let [answer (http/get url {:throw false})]
            (is (= 200 (:status answer)))
            (is (= "abcdefghij" (:body answer)))))))))

(deftest byte-range-test
  (is (= [0 99] (files/byte-range "bytes=0-99" 1000)))
  (is (= [900 999] (files/byte-range "bytes=900-" 1000)))
  (is (= :beyond (files/byte-range "bytes=1000-" 1000)))
  (testing "one answer carries at most range-bytes"
    (is (= [0 (dec files/range-bytes)] (files/byte-range "bytes=0-" (* 3 files/range-bytes)))))
  (testing "no header, or one of another shape, is no range"
    (is (nil? (files/byte-range nil 1000)))
    (is (nil? (files/byte-range "bytes=-5" 1000)))
    (is (nil? (files/byte-range "bytes=9-3" 1000)))))

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
            (testing "an empty file goes both ways"
              (spit local "")
              (is (= 0 (:exit (rep "upload" local "in/empty.bin"))))
              (is (= 0 (fs/size (fs/file (:rep/dir w) "in" "empty.bin"))))
              (is (= 0 (:exit (rep "download" "in/empty.bin" back))))
              (is (= 0 (fs/size back)))))))
      (finally
        (fs/delete-if-exists local)
        (fs/delete-if-exists back)))))
