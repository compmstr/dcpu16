(ns dcpu16.util)

(defn reverse-map [m]
  (apply hash-map (mapcat reverse m)))

(defn int->short
  "Converts a short stored in an int to a signed short"
  [num]
  (let [pos? (not (bit-test num 15))
        new-num (bit-and 0x7FFF num)]
    (* (if pos? 1 -1)
       new-num)))
(defn short->int
  "Converts a signed short to an int"
  [num]
  (let [pos? (> num 0)]
    (+ (if pos? 0 (bit-shift-left 1 15))
       (Math/abs num))))
