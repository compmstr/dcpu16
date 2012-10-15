(ns dcpu16.util)

(defn reverse-map [m]
  (apply hash-map (mapcat reverse m)))
