(ns dcpu16.codelistentry
  (:require [dcpu16.tokenval :as tokenval])
  (:use dcpu16.util)) 

;;Codelist entry types:
;;op
;;special-op
;;label
;;data

(defn codelist-entry-size
  [entry]
  (case (:type entry)
    :op
    (+ 1
       (tokenval/tokenval-size (:aval entry) true)
       (tokenval/tokenval-size (:bval entry) false)),
    :special-op
    (+ 1
       (tokenval/tokenval-size (:aval entry) true)),
    :data
    (:data-size entry)))
