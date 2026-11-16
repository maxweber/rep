(ns rep.door-test
  "The listener in front of http-kit, which is on only for who asks for
   it: a file of any size in one GET with its length and with flat
   memory, a client that hangs up, and everything else passed on to
   http-kit as it is. And the default without the door: a file above
   2 GB comes in parts."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [org.httpkit.server :as http-server]
            [rep.door :as door]
            [rep.files :as files]
            [rep.files-test :refer [megabyte random-file! sha256]]
            [rep.server :as server]
            [rep.server-test :as server-test :refer [evaluate uuid with-system wait-until]]))

(defn- download-url
  "A signed URL that gives `path`, asked for the way a caller does."
  [w path]
  (edn/read-string (:body (evaluate w (uuid) (str "(download-url " (pr-str path) ")")))))

(defn- sparse-file!
  "Makes the file `file` with `bytes` bytes of which only the first and
   the last megabyte are written, and answers those two as byte arrays.
   The megabytes between them take no room on the disk."
  [file bytes]
  (let [random (java.util.Random. 23)
        first-part (byte-array megabyte)
        last-part (byte-array megabyte)]
    (.nextBytes random first-part)
    (.nextBytes random last-part)
    (fs/create-dirs (fs/parent file))
    (with-open [out (java.io.RandomAccessFile. (str file) "rw")]
      (.setLength out bytes)
      (.write out first-part)
      (.seek out (- bytes megabyte))
      (.write out last-part))
    [first-part last-part]))

(defn- read-exactly
  "Reads `n` bytes from the stream `in`. Answers them, or fewer when the
   stream ends first."
  [in n]
  (let [out (java.io.ByteArrayOutputStream.)
        buffer (byte-array 65536)]
    (loop [left n]
      (when (pos? left)
        (let [read (.read in buffer 0 (int (min left (alength buffer))))]
          (when (pos? read)
            (.write out buffer 0 read)
            (recur (- left read))))))
    (.toByteArray out)))

(defn- skip-exactly
  "Reads `n` bytes from the stream `in` and drops them. Answers how many
   it read."
  [in n]
  (let [buffer (byte-array megabyte)]
    (loop [left n]
      (if (pos? left)
        (let [read (.read in buffer 0 (int (min left (alength buffer))))]
          (if (pos? read)
            (recur (- left read))
            (- n left)))
        n))))

(defn- resident-bytes
  "The resident memory of this process, nil where /proc has no word of
   it."
  []
  (when (fs/exists? "/proc/self/status")
    (some->> (with-open [in (java.io.FileInputStream. "/proc/self/status")]
               (String. (.readAllBytes in) "UTF-8"))
             (re-find #"VmRSS:\s+(\d+) kB")
             (second)
             (parse-long)
             (* 1024))))

(defn- open-on?
  "True when this process has `file` open, by what /proc says."
  [file]
  (let [real (str (fs/real-path file))]
    (boolean (some #(= real (try
                              (str (fs/read-link %))
                              (catch Exception _ nil)))
                   (fs/list-dir "/proc/self/fd")))))

(def ^:private door-on
  "The option that puts the door in front of http-kit."
  {:rep/door? true})

(deftest large-file-test
  (with-system door-on
    (fn [w]
      (let [bytes (* 4500 megabyte)
            stored (fs/file (:rep/dir w) "out" "large.bin")
            [first-part last-part] (sparse-file! stored bytes)
            url (download-url w "out/large.bin")]
        (testing "a HEAD says how long the file is"
          (let [answer (http/head url {:throw false})]
            (is (= 200 (:status answer)))
            (is (= (str bytes) (get (:headers answer) "content-length")))))
        (testing "a file of 4.5 GB comes down in one plain GET, with its Content-Length"
          (let [before (resident-bytes)
                answer (http/get url {:as :stream :throw false})]
            (with-open [in (:body answer)]
              (is (= 200 (:status answer)))
              (is (= (str bytes) (get (:headers answer) "content-length")))
              (is (nil? (get (:headers answer) "transfer-encoding")))
              (testing "the first and the last megabyte arrive right, and nothing after them"
                (is (= (vec first-part) (vec (read-exactly in megabyte))))
                (is (= (- bytes (* 2 megabyte)) (skip-exactly in (- bytes (* 2 megabyte)))))
                (is (= (vec last-part) (vec (read-exactly in megabyte))))
                (is (= -1 (.read in)))))
            (testing "and the server's memory stays flat while the whole file goes through it"
              (when before
                (is (< (- (resident-bytes) before) (* 256 megabyte))
                    "the resident memory grew by less than 256 MB for a file of 4500 MB")))))
        (testing "a range of it is answered as before"
          (let [answer (http/get url {:headers {"Range" (str "bytes=" (- bytes 10) "-")} :as :bytes :throw false})]
            (is (= 206 (:status answer)))
            (is (= (vec (take-last 10 last-part)) (vec (:body answer))))))))))

(deftest slices-test
  (let [local (random-file! (+ (* 3 megabyte) 1234) 31)]
    (try
      (with-system (assoc door-on :rep/slice-bytes 4096)
        (fn [w]
          (fs/create-dirs (fs/path (:rep/dir w) "out"))
          (fs/copy local (fs/path (:rep/dir w) "out" "sliced.bin"))
          (testing "with a slice of 4 KB a file of 3 MB goes out in several hundred slices and arrives intact"
            (is (= 4096 (:door/slice-bytes (:rep/door w))))
            (let [back (str local ".back")
                  answer (http/get (download-url w "out/sliced.bin") {:as :stream :throw false})]
              (try
                (with-open [in (:body answer)]
                  (fs/copy in back {:replace-existing true}))
                (is (= 200 (:status answer)))
                (is (= (str (fs/size local)) (get (:headers answer) "content-length")))
                (is (= (sha256 local) (sha256 back)))
                (finally
                  (fs/delete-if-exists back)))))
          (testing "an empty file is an answer of no bytes"
            (spit (fs/file (:rep/dir w) "out" "empty.bin") "")
            (let [answer (http/get (download-url w "out/empty.bin") {:throw false})]
              (is (= 200 (:status answer)))
              (is (= "0" (get (:headers answer) "content-length")))
              (is (= "" (:body answer)))))))
      (finally
        (fs/delete-if-exists local)))))

(defn- raw-request!
  "Opens a connection of its own to the system `w` and sends `text` as
   it is. Answers the socket."
  [w text]
  (let [socket (java.net.Socket. "127.0.0.1" (int (:rep/port w)))]
    (.write (.getOutputStream socket) (.getBytes text "ISO-8859-1"))
    (.flush (.getOutputStream socket))
    socket))

(defn- read-answer
  "Reads one answer with a Content-Length from the stream `in`: its
   status and its body as text."
  [in]
  (let [head (loop [text ""]
               (if (str/ends-with? text "\r\n\r\n")
                 text
                 (let [b (.read in)]
                   (if (neg? b)
                     text
                     (recur (str text (char b)))))))
        length (some->> (re-find #"(?i)content-length: (\d+)" head) (second) (parse-long))]
    {:status (some->> (re-find #"^HTTP/1\.1 (\d+)" head) (second) (parse-long))
     :head head
     :body (String. (read-exactly in (or length 0)) "UTF-8")}))

(deftest hang-up-test
  (with-system door-on
    (fn [w]
      (when (fs/exists? "/proc/self/fd")
        (let [stored (fs/file (:rep/dir w) "out" "large.bin")
              _ (sparse-file! stored (* 3000 megabyte))
              url (download-url w "out/large.bin")
              target (subs url (str/index-of url "/file/"))
              connections (:door/connections (:rep/door w))
              before (count @connections)
              socket (raw-request! w (str "GET " target " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"))]
          (testing "a download is under way: the door has the file open and a connection more"
            (is (= megabyte (alength (read-exactly (.getInputStream socket) megabyte))))
            (is (open-on? stored))
            (is (= (inc before) (count @connections))))
          (testing "the client hangs up in the middle: no open file and no connection, so no thread, is left behind"
            (.close socket)
            (is (wait-until #(not (open-on? stored))))
            (is (wait-until #(= before (count @connections))))))))))

(deftest passing-on-test
  (with-system door-on
    (fn [w]
      (spit (fs/file (:rep/dir w) "note.txt") "a note")
      (let [url (download-url w "note.txt")
            target (subs url (str/index-of url "/file/"))
            bearer (str "Authorization: Bearer " server-test/token "\r\n")
            evaluation (fn [code]
                         (str "POST /rep/" (uuid) "/" (uuid) " HTTP/1.1\r\nHost: 127.0.0.1\r\n" bearer
                              "Content-Length: " (count code) "\r\n\r\n" code))]
        (testing "requests for http-kit and for the door take turns on one connection, each answered in its order"
          (with-open [socket (raw-request! w (evaluation "(+ 1 2)"))]
            (let [in (.getInputStream socket)
                  out (.getOutputStream socket)
                  send! (fn [text]
                          (.write out (.getBytes text "ISO-8859-1"))
                          (.flush out))]
              (is (= {:status 200 :body "3\n"} (select-keys (read-answer in) [:status :body])))
              (send! (str "GET " target " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"))
              (is (= {:status 200 :body "a note"} (select-keys (read-answer in) [:status :body])))
              (send! (evaluation "(* 2 21)"))
              (is (= {:status 200 :body "42\n"} (select-keys (read-answer in) [:status :body])))
              (send! (str "GET " target " HTTP/1.1\r\nHost: 127.0.0.1\r\nRange: bytes=2-5\r\n\r\n"))
              (is (= {:status 206 :body "note"} (select-keys (read-answer in) [:status :body])))
              (send! (str "HEAD " target " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"))
              (let [head (loop [text ""]
                           (let [b (.read in)]
                             (if (neg? b)
                               text
                               (recur (str text (char b))))))]
                (is (str/starts-with? head "HTTP/1.1 200 OK\r\n"))
                (is (str/includes? head "Content-Length: 6\r\n") "a HEAD says the length")
                (is (str/includes? head "Connection: close\r\n"))
                (is (str/ends-with? head "\r\n\r\n") "and has no body, and the door closed as the client asked")))))
        (testing "a request with a chunked body is passed on whole"
          (with-open [socket (raw-request! w (str "POST /rep/" (uuid) "/" (uuid) " HTTP/1.1\r\nHost: 127.0.0.1\r\n" bearer
                                                  "Transfer-Encoding: chunked\r\n\r\n"
                                                  "4\r\n(+ 1\r\n3\r\n 2)\r\n0\r\n\r\n"))]
            (is (= {:status 200 :body "3\n"} (select-keys (read-answer (.getInputStream socket)) [:status :body])))))
        (testing "what is no HTTP request gets a 400, a head that does not end a 431, and both a closed connection"
          (with-open [socket (raw-request! w "hello there\r\n\r\n")]
            (is (= 400 (:status (read-answer (.getInputStream socket)))))
            (is (= -1 (.read (.getInputStream socket)))))
          (with-open [socket (raw-request! w (str "GET / HTTP/1.1\r\nX-Long: " (apply str (repeat 70000 "x"))))]
            (is (= 431 (:status (read-answer (.getInputStream socket)))))))
        (testing "a signed GET without its signature is refused by the handler the door asks"
          (is (= 403 (:status (http/get (str/replace url #"signature=[^&]+" "signature=wrong") {:throw false})))))))))

(deftest parse-head-test
  (is (= {:door/method "GET"
          :door/version "HTTP/1.1"
          :uri "/file/a%20b.txt"
          :query-string "expires=1&signature=s"
          :headers {"host" "machine.example" "range" "bytes=0-9" "accept" "a,b"}}
         (dissoc (door/parse-head (str "GET /file/a%20b.txt?expires=1&signature=s HTTP/1.1\r\n"
                                       "Host: machine.example\r\nRange:  bytes=0-9 \r\nAccept: a\r\nAccept: b\r\n\r\n"))
                 :door/head)))
  (is (nil? (:query-string (door/parse-head "GET / HTTP/1.0\r\n\r\n"))))
  (is (nil? (door/parse-head "hello there\r\n\r\n")))
  (is (nil? (door/parse-head "GET / HTTP/2.0\r\n\r\n"))))

(defn- above-two-gigabytes-test
  "What a server without the door answers for the file `out/large.bin`
   of 2.5 GB behind `url`, whose first megabyte is `first-part`."
  [url first-part]
  (let [bytes (* 2500 megabyte)]
    (testing "a plain GET gets a 400 that names the size and says how to fetch the file, not http-kit's 500"
      (let [answer (http/get url {:throw false})]
        (is (= 400 (:status answer)))
        (is (str/includes? (:body answer) (str "out/large.bin holds " bytes " bytes")))
        (is (str/includes? (:body answer) "above 2 GB in parts only"))
        (is (str/includes? (:body answer) "Range header"))
        (is (str/includes? (:body answer) "curl -r"))))
    (testing "a HEAD gets the status of that GET"
      (is (= 400 (:status (http/head url {:throw false})))))
    (testing "a Range gets its part, from the start and from behind the 2 GB"
      (let [answer (http/get url {:headers {"Range" "bytes=0-9"} :as :bytes :throw false})]
        (is (= 206 (:status answer)))
        (is (= (str "bytes 0-9/" bytes) (get (:headers answer) "content-range")))
        (is (= (vec (take 10 first-part)) (vec (:body answer)))))
      (let [start (* 2400 megabyte)
            answer (http/get url {:headers {"Range" (str "bytes=" start "-" (+ start 9))} :as :bytes :throw false})]
        (is (= 206 (:status answer)))
        (is (= (str "bytes " start "-" (+ start 9) "/" bytes) (get (:headers answer) "content-range")))
        (is (= (repeat 10 0) (vec (:body answer))))))))

(deftest default-test
  (testing "the door is off unless asked for"
    (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))]
      (try
        (is (false? (:rep/door? (server/prepare {:rep/dir folder :rep/token server-test/token}))))
        (finally
          (fs/delete-tree folder))))
    (is (= {:rep/door? true} (server/cli-options ["--door"])))
    (is (= {} (server/cli-options []))))
  (with-system {}
    (fn [w]
      (testing "without the door http-kit itself has the port"
        (is (nil? (:rep/door w)))
        (is (= (:rep/port w) (http-server/server-port (:rep/server w)))))
      (testing "a file below 2 GB comes whole in one GET, with its length"
        (spit (fs/file (:rep/dir w) "note.txt") "a note")
        (let [answer (http/get (download-url w "note.txt") {:throw false})]
          (is (= 200 (:status answer)))
          (is (= "6" (get (:headers answer) "content-length")))
          (is (= "a note" (:body answer))))
        (let [answer (http/head (download-url w "note.txt") {:throw false})]
          (is (= 200 (:status answer)))
          (is (= "6" (get (:headers answer) "content-length")))))
      (let [[first-part _] (sparse-file! (fs/file (:rep/dir w) "out" "large.bin") (* 2500 megabyte))]
        (above-two-gigabytes-test (download-url w "out/large.bin") first-part)))))

(deftest host-without-the-door-test
  (testing "a host that mounts the handler in its own http-kit answers the same"
    (let [folder (str (fs/create-temp-dir {:prefix "rep-test"}))
          system (server/prepare {:rep/dir folder :rep/token server-test/token})
          host (http-server/run-server (fn [request] (server/handle system request))
                                       {:port 0 :ip "127.0.0.1" :legacy-return-value? false})
          [first-part _] (sparse-file! (fs/file folder "out" "large.bin") (* 2500 megabyte))
          url (files/url system {:url/method "GET" :url/path "out/large.bin" :url/seconds 60
                                 :url/base (str "http://127.0.0.1:" (http-server/server-port host))})]
      (try
        (above-two-gigabytes-test url first-part)
        (finally
          (http-server/server-stop! host)
          (server/stop! system)
          (fs/delete-tree folder))))))
