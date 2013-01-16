(ns dcpu16.parser
  (:require [clojure.string :only [trim, split] :as string]
            [dcpu16.codes :as codes]
            [dcpu16.codelistentry :as cle]
            [dcpu16.tokenval :as tokenval])
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

(defn dat-get-nums
  [s]
  (map #(bit-and 0xFFFF (Long/decode %))
       (string/split s #"\s+")))
(defn dat-convert-string
  [s]
  (loop [accum []
         subs s]
    (if (empty? subs)
      accum
      (let [cur (map int (take 2 subs))
            val (+ (first cur)
                   (if (= (count cur) 1)
                     0
                     (bit-shift-left (second cur) 8)))]
        (recur (conj accum val)
               (drop 2 subs))))))
(defn dat-get-string
  [s]
  (dat-convert-string
   (second (re-find #"^\"(.+)\"$" (string/trim s)))))
(defn dat-convert
  "Extracts an array of unsigned shorts from passed in data
   Handles either a list of integer literals or a string, atm"
  [s]
  (cond
   (not (= -1 (.indexOf #^String s (int \")))) (dat-get-string s)
   true (dat-get-nums s)))
(defn process-dat
  [s codelist]
  ;;take off the "dat" at the start of the line
  (let [line (string/trim (subs s 3))
        data (dat-convert line)
        data-size (count data)]
    (assoc codelist
      :code-entries
      (conj (:code-entries codelist)
            {:type :data
             :data data
             :data-size data-size})
      :pos
      (+ (:pos codelist) data-size))))
(defn process-label
  [s codelist]
  ;;strip off the : at the start of the label
  (let [label-name (subs s 1)]
    (update-in codelist
               [:labels]
               #(conj % (Label. label-name (:pos codelist))))))
(defn find-label
  "Given a list of labels, and a name to search for, returns the named label"
  [labels name]
  (loop [sublst labels]
    (if (empty? sublst)
      nil
      (if (= (:name (first sublst)) name)
        (first sublst)
        (recur (rest sublst))))))
(defn op-type
  [op-name]
  (let [op-key (keyword op-name)]
    (cond
     (contains? codes/ops op-key) :op
     (contains? codes/special-ops op-key) :special-op
     :default nil)))
(defn process-op
  [parts]
  (let [type (op-type (first parts))
        base {:type type type (keyword (first parts))}]
    (merge base
      (case (op-type (first parts))
        :op {:aval (tokenval/get-val (nth parts 2))
             :bval (tokenval/get-val (nth parts 1))}
        :special-op {:aval (tokenval/get-val (nth parts 1))}
        nil (throw (IllegalArgumentException. (str "Invalid op: " (first parts))))))))
(defn split-op-line
  [s]
  (map #(if (.endsWith #^String % ",")
          (apply str (butlast %))
          %)
       (string/split s #"\s+")))
(defn process-line
  [s codelist]
  (let [entry (process-op (split-op-line s))]
    (assoc codelist
      :code-entries
      (conj (:code-entries codelist)
            entry)
      :pos
      (+ (:pos codelist) (cle/codelist-entry-size entry)))))
            

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

(defn process-label-vals
"Passed in a parse tree from parse-file, updates all of the a/b values that
are labels or label-mems with the actual label values"
  [parse-tree]
  (let [labels (:labels parse-tree)
        process-label-val (fn [val]
                            (if (or
                                 (= (:type val) :label)
                                 (= (:type val) :label-mem))
                              (assoc val :val
                                     (:code-loc (find-label labels (:label val))))
                              val))]
    (assoc parse-tree :code-entries (map (fn [entry]
                                           (case (:type entry)
                                             :op
                                             (update-in
                                              (update-in entry [:aval] process-label-val)
                                              [:bval] process-label-val)
                                             :special-op
                                             (update-in entry [:aval] process-label-val )
                                             entry)) ;; default is return entry
                                         (:code-entries parse-tree)))))