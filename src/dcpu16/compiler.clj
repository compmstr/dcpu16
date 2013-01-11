(ns dcpu16.compiler
  (:require [dcpu16.parser :as parser]
            [dcpu16.encoder :as encoder])
  (:use dcpu16.util))

(defn write-code
  "Expects a stream to write to and an array of unsigned short values"
  [out data])

(defn compile-file
  [#^String in-file #^String out-file]
  (let [codelist (parser/parse-file in-file)]
    (with-open [out (java.io.FileOutputStream. out-file)]
      (loop [code codelist]
        (when code
          (write-code out (encoder/encode (first code)))
          (recur (rest code)))))))