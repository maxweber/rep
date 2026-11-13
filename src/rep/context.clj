(ns rep.context
  "The SCI context of a session and the functions its code may call. The
   context is closed. It has no Java classes and no file or process
   access of its own. What reaches the machine goes through the functions
   listed in `bindings`, plus the namespaces the host adds."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [rep.files :as files]
            [rep.output :as output]
            [rep.shell :as shell]
            [sci.core :as sci]))

(def ^:dynamic *evaluation*
  "The evaluation that runs on this thread."
  nil)

(defn quiet
  "Marks `value` as one whose text is in the output already. The server
   does not print it a second time when it is the value of the code."
  [value]
  (vary-meta value assoc :rep/quiet true))

(defn quiet?
  [value]
  (boolean (:rep/quiet (meta value))))

(defn sh
  "Runs `script` with bash in the work folder. The output goes into the
   answer as bash wrote it. The value is `{:exit <code>}`."
  [{:rep/keys [dir]} script]
  (let [{:evaluation/keys [output process]} *evaluation*]
    (flush)
    (-> {:shell/script (str script)
         :shell/dir dir
         :shell/write! (fn [data offset length]
                         (output/write! output data offset length))
         :shell/started! (fn [running]
                           (reset! process running))}
        (shell/bash!)
        (select-keys [:shell/exit])
        (update-keys (comp keyword name))
        (quiet))))

(defn- signed-url
  [{:rep/keys [url-seconds] :as w} method path seconds]
  (files/url w {:url/method method
                :url/path path
                :url/seconds (or seconds url-seconds)
                :url/base (:evaluation/base-url *evaluation*)}))

(defn- write-file
  [w path content options]
  (let [target (files/file w path)]
    (io/make-parents target)
    (apply spit target content options)))

(declare help)

(defn bindings
  "What the code of a session finds in its `user` namespace."
  [w]
  [{:name 'sh
    :arglists '([script])
    :doc "Runs the bash script in the work folder. Its output, standard error included, goes into the answer as bash wrote it. Answers {:exit <code>}."
    :function (fn [script] (sh w script))}
   {:name 'slurp
    :arglists '([path])
    :doc "The text of a file in the work folder."
    :function (fn [path] (slurp (files/file w path)))}
   {:name 'spit
    :arglists '([path content & options])
    :doc "Writes content into a file in the work folder, folders made as needed. Options as in Clojure, such as :append true."
    :function (fn [path content & options] (write-file w path content options))}
   {:name 'upload-url
    :arglists '([path] [path seconds])
    :doc "A URL for an HTTP PUT whose body becomes the file at path in the work folder. It needs no token and expires."
    :function (fn
                ([path] (signed-url w "PUT" path nil))
                ([path seconds] (signed-url w "PUT" path seconds)))}
   {:name 'download-url
    :arglists '([path] [path seconds])
    :doc "A URL that answers an HTTP GET with the file at path in the work folder. It needs no token and expires."
    :function (fn
                ([path] (signed-url w "GET" path nil))
                ([path seconds] (signed-url w "GET" path seconds)))}
   {:name 'help
    :arglists '([])
    :doc "Prints what the code of this session may call."
    :function (fn [] (help w))}])

(defn help
  "Prints the functions of the session, one line each, and the namespaces
   the host added."
  [w]
  (doseq [{:keys [name arglists doc]} (bindings w)]
    (println (str/join " " (map #(pr-str (cons name %)) arglists)))
    (println (str "  " doc)))
  (when-let [added (seq (sort (keys (:rep/namespaces w))))]
    (println (str "Namespaces added here: " (str/join ", " added) ".")))
  (println "(clojure.repl/doc f) explains a function, (clojure.repl/dir user) lists a namespace.")
  (println "Definitions stay for the session. Every request starts in the namespace user.")
  (println "The value of the code is printed as EDN, nil is not. (print text) gives text as it is."))

(defn create
  "A fresh context for one session."
  [w]
  (let [user (sci/create-ns 'user nil)
        own (into {}
                  (map (fn [{:keys [name function] :as binding}]
                         [name (sci/new-var name function
                                            (assoc (select-keys binding [:doc :arglists])
                                                   :ns user))]))
                  (bindings w))]
    (sci/init {:namespaces (merge-with merge
                                       {'user own
                                        'clojure.pprint {'pprint pprint/pprint}}
                                       (:rep/namespaces w))})))

(comment
  (def ctx (create {:rep/dir "work" :rep/secret "secret" :rep/url-seconds 900}))
  (sci/eval-string* ctx "(def a 1)")
  (sci/eval-string* ctx "(inc a)")
  (sci/eval-string* ctx "(clojure.repl/doc sh)"))
