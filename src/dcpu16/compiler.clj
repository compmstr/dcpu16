(ns dcpu16.compiler
  (:require [clojure.string :only [trim, split] :as string])
  (:use dcpu16.util))

;;For REPL, hide array elements after 100
(set! *print-length* 100)

;;Storage for labels found in the code
(defrecord Label [name code-loc])

;;Empty codelist
(def new-codelist
  {:size 0
   :code-entries []
   :labels []})

(defn line-blank?
  [#^String s]
  (every? #(Character/isWhitespace %) s))
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

(defn reset-compiler
  [])

(defn process-dat
  [s codelist])
(defn process-label
  [s codelist])
(defn process-line
  [s codelist])

(declare parse-file)
(defn parse-line
  [#^String s codelist]
  (cond
   (line-blank? s) codelist
   (line-comment? s) codelist
   (line-dat? s) (process-dat s codelist)
   (line-label? s) (process-label s codelist)
   (line-include? s) (parse-file codelist)
   :default (process-line s codelist)))

(defn parse-file
  "Parse a file, returning a vector of code list entries
   If a codelist is passed in, adds code into the provided codelist"
  ([filename]
     (parse-file filename new-codelist))
  ([filename codelist]))