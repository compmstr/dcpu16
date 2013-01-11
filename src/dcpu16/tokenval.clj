(ns dcpu16.tokenval
  (:use dcpu16.util)
  (:require [clojure.string :as string]))

;;Value types:
;;reg
;;reg-mem
;;reg-mem-next
;;pushpop
;;peek
;;pick
;;sp
;;pc
;;ex
;;mem-next
;;lit

(defn val-type
  [raw-token]
  (let [token (string/upper-case raw-token)]
    (cond
     (and (= (count token) 1)
          (contains? (set "ABCXYZIJ") (first token)))
     :reg,
     (and (square-bracketed? token)
          (= :reg (val-type (strip-brackets token))))
     :reg-mem,
     (re-find #"\[[ABCXYZIJ]\+(0X)?\d+\]" token)
     :reg-mem-next,
     (re-find #"PUSH|POP" token)
     :pushpop,
     (= "PEEK" token)
     :peek,
     (re-find #"PICK\s+\d+" token)
     :pick,
     (= "SP" token)
     :sp,
     (= "PC" token)
     :pc,
     (= "EX" token)
     :ex,
     (and (square-bracketed? token)
          (re-find #"^(0X)?\d+$" (strip-brackets token)))
     :mem-next,
     (re-find #"^(0X)?\d+$" token)
     :lit,
     true
     :label)))

(defn get-val
  [raw-token]
  (let [token (string/upper-case raw-token)
        type (val-type token)
        base {:type type}]
    (case type
      :reg (assoc base
             :reg (keyword token))
      :reg-mem (assoc base
                 :reg (keyword (strip-brackets token)))
      :reg-mem-next (let [re-chunks (re-find #"\[([ABCXYZIJ])\+((0X)?\d+)\]" token)]
                      (assoc base
                        :reg (keyword (second re-chunks))
                        :val (Integer/decode (nth re-chunks 2))))
      :pushpop base
      :peek base
      :pick (assoc base
              :val (Integer/decode (second (re-find #"^PICK\s+((0X)?\d+)$" token))))
      :sp base
      :pc base
      :ex base
      :mem-next (assoc base
                  :val (Integer/decode (strip-brackets token)))
      :lit (assoc base
             :val (Integer/decode token))
      :label (assoc base
               :label token))))

(defn tokenval-size
  "if this value requires extra storage, returns 1, else returns 0"
 [val a?]
 (case (:type val)
   :mem-next 1
   :lit (if (and a?
                 (<= (:val val) 0x1f))
          0
          1)
   :reg-mem-next 1
   :pick 1
   0))
