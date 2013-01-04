(ns dcpu16.parser
  (:require [clojure.string :only [trim, split] :as string])
  (:use dcpu16.util))

;;For REPL, hide array elements after 100
(set! *print-length* 100)

(declare parse-file)
;;Storage for labels found in the code
(defrecord Label [name code-loc])

;;Empty codelist
(def new-codelist
  {:pos 0
   :files []
   :code-entries []
   :labels []})

(defn line-blank?
  [#^String s]
  (every? #(Character/isWhitespace #^Character %) s))
(defn line-comment?
  [#^String s]
  (line-starts? s ";"))
(defn line-include?
  [#^String s]
  (line-starts? s "#include"))
(defn line-dat?
  [#^String s]
  (line-starts? s "dat"))
(defn line-label?
  [#^String s]
  (line-starts? s ":"))

(defn process-dat
  [s codelist]
  codelist)
(defn process-label
  [s codelist]
  ;;strip off the : at the start of the label
  (let [label-name (subs s 1)]
    (update-in codelist
               [:labels]
               #(conj % (Label. label-name (:pos codelist))))))
(defn process-line
  [s codelist]
  codelist)

(defn- get-include-filename
  [s]
  (string/trim (subs s (count "#include"))))
(defn process-include
  [s codelist]
  (let [filename (get-include-filename s)]
    (println "Including file: " filename)
    (println "files: " (:files codelist))
    (let [search (make-path
                  (get-directory (last (:files codelist)))
                  filename)]
    (cond
     (file-exists? filename) (parse-file filename codelist)
     (file-exists? search) (parse-file search codelist)))))

(defn parse-line
  [#^String s codelist]
  (cond
   (line-blank? s) codelist
   (line-comment? s) codelist
   (line-dat? s) (process-dat s codelist)
   (line-label? s) (process-label s codelist)
   (line-include? s) (process-include s codelist)
   :default (process-line s codelist)))

(defn parse-file
  "Parse a file, returning a vector of code list entries
   If a codelist is passed in, adds code into the provided codelist"
  ([filename]
     (parse-file filename new-codelist))
  ([filename codelist]
     (println "Compiling file: " filename)
     (with-open [rdr (clojure.java.io/reader filename)]
       (loop [cl (update-in codelist
                            [:files]
                            #(conj % filename))
              lines (map string/trim (line-seq rdr))]
         (if (empty? lines)
           (update-in cl
                      [:files]
                      pop)
           (recur
            (parse-line (first lines) cl)
            (rest lines)))))))