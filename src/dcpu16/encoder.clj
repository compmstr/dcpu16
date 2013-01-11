(ns dcpu16.encoder
  (:require [dcpu16.codes :as codes]
            [dcpu16.tokenval :as tokenval])
  (:use dcpu16.util))

(defn encode-op
  [entry])
(defn encode-special-op
  [entry])

(defn encode
  "Takes in a codelist entry, returns a list of unsigned shorts to write out"
  [entry]
  (case (:type entry)
    :op (encode-op entry)
    :special-op (encode-special-op entry)
    :data (:data entry)
    true []))