(ns dcpu16.encoder
  (:require [dcpu16.codes :as codes]
            [dcpu16.tokenval :as tokenval])
  (:use dcpu16.util))

(defn encode-op
  [entry]
  (let [aval (tokenval/encode-tokenval (:aval entry) true)
        bval (tokenval/encode-tokenval (:bval entry) false)]
    (concat 
      [(+ (codes/ops (:op entry))
          (bit-shift-left (first aval) 10)
          (bit-shift-left (first bval) 5))]
      (rest aval)
      (rest bval))))
(defn encode-special-op
  [entry]
  (let [aval (tokenval/encode-tokenval (:aval entry) true)]
    (concat [(+ (bit-shift-left (codes/special-ops (:special-op entry)) 5)
                (bit-shift-left (first aval) 10))]
            (rest aval))))

(defn encode
  "Takes in a codelist entry, returns a list of unsigned shorts to write out"
  [entry]
  (case (:type entry)
    :op (encode-op entry)
    :special-op (encode-special-op entry)
    :data (:data entry)
    true []))