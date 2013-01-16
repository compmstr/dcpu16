(ns dcpu16.compiler
  (:require [dcpu16.parser :as parser]
            [dcpu16.encoder :as encoder])
  (:use dcpu16.util))

(defn write-code
  "Writes the unsigned short data to the stream out"
  [out data]
  (.write #^java.io.FileOutputStream out #^Integer (bit-and 0xFF data))
  (.write #^java.io.FileOutputStream out #^Integer (bit-and 0xFF (bit-shift-right data 8))))

(defn compile-file
  [#^String in-file #^String out-file]
  (let [codelist (parser/process-label-vals (parser/parse-file in-file))
        encoded (flatten (map encoder/encode (:code-entries codelist)))]
    (with-open [out (java.io.FileOutputStream. out-file)]
      (doseq [word encoded]
        (write-code out word)))))