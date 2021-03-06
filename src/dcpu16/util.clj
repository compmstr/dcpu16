(ns dcpu16.util
  (:require [clojure.string :as string :only [split trim join]]))

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
  (let [pos? (>= num 0)]
    (+ (if pos? 0 (bit-shift-left 1 15))
       (Math/abs ^Integer num))))

(defn swap-word-bytes
  [num]
  (let [b1 (bit-shift-right (bit-and 0xFF00 num) 8)
        b2 (bit-and 0xFF num)]
    (+ b1 (bit-shift-left b2 8))))

(defn line-starts?
  "Checks if a line starts with a particular string, ignoring
   leading whitespace"
  [#^String line #^String match]
  (.startsWith (string/trim line) (string/trim match)))

(defn line-quoted-string
  "Gets a quoted string from the passed in line"
  [s]
  (re-find #"\".+\"" s))

(defn strip-brackets
  [s]
  (apply str (butlast (rest s))))
(defn square-bracketed?
  [s]
  (and (= (first s) \[)
       (= (last s) \])))

(defn file-exists?
  [s]
  (.exists (java.io.File. #^String s)))

(defn get-directory
  [s]
  (.getParent (java.io.File. #^String s)))

(defn make-path
  [& items]
  (string/join
   java.io.File/separator
   items))

(defn within?
  [val low high]
  (and (>= val low)
       (<= val high)))

(defn hex-string
  [num]
  (format "0x%04x" (short->int num)))

(defn binary-string
  [num]
  (Integer/toBinaryString num))