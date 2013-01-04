(ns dcpu16.encoder
  (:use dcpu16.util)) 

;;Value types:
reg
reg-mem
reg-mem-next
pushpop
peek
pick
sp
pc
ex
mem-next
lit

(defn value-extra-size
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