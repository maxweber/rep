(ns rep.files-test
  "Files that are larger than one request: a body the server refuses,
   an upload and a download in parts, and the offsets of a part."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

(defn- upload-url
  "A signed URL that takes `path`, asked for the way a caller does."
  [w path]
  (edn/read-string (:body (evaluate w (uuid) (str "(upload-url " (pr-str path) ")")))))

(defn- download-url
  "A signed URL that gives `path`, asked for the way a caller does."
  [w path]
  (edn/read-string (:body (evaluate w (uuid) (str "(download-url " (pr-str path) ")")))))

(defn- put
  "PUTs `body` to the signed URL, as the part at `offset` when given."
  ([url body]
   (http/put url {:body body :throw false}))
  ([url body offset]
   (http/put (str url "&offset=" offset) {:body body :throw false})))

(defn- size-header
  [response]
  (get (:headers response) (str/lower-case files/size-header)))

(defn- put-parts!
  "Sends the file `local` to `url` in parts of `part-bytes`, each at the
   offset the server said the file holds. Answers the bytes the file
   holds at the end."
  [url local part-bytes]
  (with-open [in (java.io.RandomAccessFile. (str local) "r")]
    (loop [offset 0]
      (let [part (byte-array (min part-bytes (- (.length in) offset)))
            _ (.seek in offset)
            _ (.readFully in part)
            held (parse-long (size-header (put url part offset)))]
        (if (< held (.length in))
          (recur held)
          held)))))

(defn- get-parts!
  "Fetches the file behind `url` into the local file `back`, one range
   after the other, each as long as the server makes it. Answers the
   bytes that came."
  [url back]
  (with-open [out (java.io.FileOutputStream. (str back))]
    (loop [offset 0]
      (let [answer (http/get url {:headers {"Range" (str "bytes=" offset "-")} :as :bytes :throw false})
            total (parse-long (second (re-find #"/(\d+)$" (get (:headers answer) "content-range"))))
            end (+ offset (alength (:body answer)))]
        (.write out ^bytes (:body answer))
        (if (< end total)
          (recur end)
          end)))))

(deftest links-test
  (with-system {}
    (fn [w]
      (let [work (:rep/dir w)
            session (uuid)
            url-for (fn [function path]
                      (edn/read-string (:body (evaluate w session (str "(" function " " (pr-str path) ")")))))
            secret (slurp (fs/file (:rep/state-dir w) "secret"))]
        (fs/create-dirs (fs/path work "in"))
        (spit (fs/file work "in" "real.txt") "a real file")
        (fs/create-sym-link (fs/path work "in" "file-link") (fs/path ".." ".rep" "secret"))
        (fs/create-sym-link (fs/path work "in" "folder-link") (fs/path ".." ".rep"))
        (fs/create-sym-link (fs/path work "in" "inside-link") "real.txt")
        (fs/create-sym-link (fs/path work "in" "nowhere-link") (fs/path ".." ".rep" "not-there-yet"))
        (testing "a link inside the work folder to a file of the state folder is refused, the signing key stays in"
          (let [answer (http/get (url-for "download-url" "in/file-link") {:throw false})]
            (is (= 403 (:status answer)))
            (is (not (str/includes? (:body answer) secret)))))
        (testing "so is a path through a link to the state folder itself"
          (let [answer (http/get (url-for "download-url" "in/folder-link/secret") {:throw false})]
            (is (= 403 (:status answer)))
            (is (not (str/includes? (:body answer) secret))))
          (is (= 403 (:status (http/get (url-for "download-url" "in/folder-link/tokens") {:throw false})))))
        (testing "a write through such a link is refused and changes nothing"
          (is (= 403 (:status (http/put (url-for "upload-url" "in/file-link") {:body "overwritten" :throw false}))))
          (is (= 403 (:status (http/put (url-for "upload-url" "in/folder-link/planted") {:body "planted" :throw false}))))
          (is (= 403 (:status (http/put (str (url-for "upload-url" "in/folder-link/planted") "&offset=0&upload=x")
                                        {:body "planted" :throw false}))))
          (is (= secret (slurp (fs/file (:rep/state-dir w) "secret"))))
          (is (not (fs/exists? (fs/path (:rep/state-dir w) "planted")))))
        (testing "a link that leads nowhere is refused, a write through it would make a file where it points"
          (is (= 403 (:status (http/put (url-for "upload-url" "in/nowhere-link") {:body "x" :throw false}))))
          (is (not (fs/exists? (fs/path (:rep/state-dir w) "not-there-yet")))))
        (testing "a link that stays inside the work folder works as before"
          (is (= "a real file" (:body (http/get (url-for "download-url" "in/inside-link") {:throw false}))))
          (is (= 201 (:status (http/put (url-for "upload-url" "in/inside-link") {:body "written through" :throw false}))))
          (is (= "written through" (slurp (fs/file work "in" "real.txt")))))
        (testing "a link that leads out of the work folder is refused, as before"
          (fs/create-sym-link (fs/path work "in" "way-out") (fs/parent work))
          (is (= 403 (:status (http/get (url-for "download-url" "in/way-out/anything") {:throw false}))))
          (is (= 403 (:status (http/put (url-for "upload-url" "in/way-out/planted.txt") {:body "x" :throw false})))))))))

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
            (testing "20 MB go up in parts of 8 MB and arrive intact"
              (is (= (* 20 megabyte) (put-parts! (upload-url w "in/big.bin") local (* 8 megabyte))))
              (is (= (* 20 megabyte) (fs/size stored)))
              (is (= (sha256 local) (sha256 stored))))
            (testing "and come back in parts, 8 MB a range"
              (is (= (* 20 megabyte) (get-parts! (download-url w "in/big.bin") back)))
              (is (= (sha256 local) (sha256 back))))
            (testing "the same upload again starts the file over, it does not append"
              (is (= (* 20 megabyte) (put-parts! (upload-url w "in/big.bin") local (* 8 megabyte))))
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

(defn- stored-at
  "Hands the part `body` of the upload `id` at `offset` to `store!`
   itself, with the clock at `now-ms`. `url` is a signed URL of the
   system `w`. Answers the status."
  [w url body offset id now-ms]
  (let [[_ uri query] (re-find #"^https?://[^/]+(/[^?]*)\?(.*)$" url)]
    (:status (files/store! (assoc w :rep/now-ms now-ms)
                           {:request-method :put
                            :uri uri
                            :query-string (str query "&offset=" offset (when id (str "&upload=" id)))
                            :headers {}
                            :body (java.io.ByteArrayInputStream. (.getBytes body "UTF-8"))}))))

(deftest upload-id-test
  (with-system {}
    (fn [w]
      (let [url (upload-url w "in/shared.txt")
            stored (fs/file (:rep/dir w) "in" "shared.txt")
            refused? (fn [answer start]
                       (and (= 409 (:status answer))
                            (nil? (size-header answer))
                            (str/starts-with? (:body answer) start)))]
        (testing "the part at offset 0 puts its upload on record, and the upload goes on"
          (is (= 201 (:status (put-as url "aaa" 0 "upload-a"))))
          (is (= 201 (:status (put-as url "aaa" 3 "upload-a"))))
          (is (= "aaaaaa" (slurp stored))))
        (testing "a part of another upload is a 409 that says so and changes nothing, even at the right offset"
          (is (refused? (put-as url "bbb" 6 "upload-b") "Another upload writes in/shared.txt."))
          (is (= "aaaaaa" (slurp stored))))
        (testing "a part without an id does not pass into an upload that has one, at the right offset or at 0"
          (is (refused? (put url "ccc" 6) "An upload writes in/shared.txt, and this part names none."))
          (is (refused? (put url "ccc" 0) "An upload writes in/shared.txt, and this part names none."))
          (is (refused? (put url "ccc") "An upload writes in/shared.txt, and this part names none.")
              "a PUT of the whole file is such a part too")
          (is (= "aaaaaa" (slurp stored))))
        (testing "the upload on record goes on as if nothing happened"
          (is (= "9" (size-header (put-as url "aaa" 6 "upload-a")))))
        (testing "with resume=true another upload takes the file over, and the first one is told"
          (is (= 201 (:status (put-as url "bbb" 9 "upload-b" "&resume=true"))))
          (is (refused? (put-as url "aaa" 12 "upload-a") "Another upload writes in/shared.txt."))
          (is (= 201 (:status (put-as url "bbb" 12 "upload-b"))))
          (is (= "aaaaaaaaabbbbbb" (slurp stored))))
        (testing "a wrong offset of the right upload is still the 409 that says the size"
          (is (= "15" (size-header (put-as url "bbb" 99 "upload-b")))))
        (testing "offset 0 with an id starts the file over for whoever sends it, and the upload before is told"
          (is (= 201 (:status (put-as url "ddd" 0 "upload-d"))))
          (is (refused? (put-as url "bbb" 3 "upload-b") "Another upload writes in/shared.txt."))
          (is (= "ddd" (slurp stored))))
        (testing "the record is beside rep's own files, not among the work folder's"
          (is (= ["shared.txt"] (map fs/file-name (fs/list-dir (fs/parent stored)))))
          (is (= 1 (count (fs/list-dir (fs/path (:rep/state-dir w) "uploads")))))))))
  (testing "a part with an id above offset 0 needs a record with that id: a file without a record refuses it"
    (with-system {}
      (fn [w]
        (let [url (upload-url w "in/plain.txt")
              stored (fs/file (:rep/dir w) "in" "plain.txt")]
          (is (= 201 (:status (put url "aaa" 0))) "a part without an id, and so without a record")
          (is (= 201 (:status (put url "aaa" 3))) "which goes on as before while no upload is on record")
          (let [answer (put-as url "bbb" 6 "upload-b")]
            (is (= 409 (:status answer)))
            (is (nil? (size-header answer)))
            (is (str/starts-with? (:body answer) "No upload with this id writes in/plain.txt.")))
          (is (= "aaaaaa" (slurp stored)))
          (testing "with resume=true it takes the file over, and from then on a part without an id is refused"
            (is (= 201 (:status (put-as url "bbb" 6 "upload-b" "&resume=true"))))
            (is (= 409 (:status (put url "ccc" 9))))
            (is (= "aaaaaabbb" (slurp stored))))))))
  (testing "an upload that sent no part for 15 minutes is over"
    (with-system {}
      (fn [w]
        (let [url (upload-url w "in/idle.txt")
              stored (fs/file (:rep/dir w) "in" "idle.txt")
              now (System/currentTimeMillis)
              minutes #(+ now (* % 60 1000))]
          (is (= 201 (stored-at w url "aaa" 0 "upload-a" now)))
          (is (= 201 (stored-at w url "aaa" 3 "upload-a" (minutes 14))) "a part within the 15 minutes keeps it on record")
          (is (= 409 (stored-at w url "ccc" 6 nil (minutes 28))) "and counts from that part on")
          (testing "then its own next part is refused like a stranger's, it has to resume"
            (is (= 409 (stored-at w url "aaa" 6 "upload-a" (minutes 30)))))
          (testing "and a part without an id is taken again"
            (is (= 201 (stored-at w url "ccc" 6 nil (minutes 30))))
            (is (= "aaaaaaccc" (slurp stored))))
          (testing "the sweep deletes the records of uploads that are over, and no other"
            (is (= 201 (stored-at w url "ddd" 0 "upload-d" (minutes 40))))
            (is (= 0 (files/forget-uploads! (assoc w :rep/now-ms (minutes 50)))))
            (is (= 1 (files/forget-uploads! (assoc w :rep/now-ms (minutes 56)))))
            (is (empty? (fs/list-dir (fs/path (:rep/state-dir w) "uploads"))))))))))

