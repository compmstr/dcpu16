(ns dcpu16.codelistentry
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
       (tokenval-size (:aval entry) true)
       (tokenval-size (:bval entry) false)),
    :special-op
    (+ 1
       (tokenval-size (:aval entry) true)),
    :label
    0,
    :data
    (:data-size entry)))
