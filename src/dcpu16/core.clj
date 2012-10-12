(ns dcpu16.core)

;;0x10000 words of ram
;;  Each word is unsigned, so using ints
(defonce ram (int-array 0x10000))
(defn ram-set
  [loc val]
  (aset-int ram loc val))
(defn ram-get
  [loc]
  (aget ram loc))

;;Registers (A, B, C, X, Y, Z, I, J, O - overflow)
;;  Again, unsigned words, so use ints
(def registers (int-array 9))
(def register-names [:A :B :C :X :Y :Z :U :J :O])
(def reg->idx (apply hash-map
                     (interleave
                      register-names
                      (range))))
(def idx->reg (apply hash-map
                     (interleave
                      (range)
                      register-names)))

(defn register-index
  "If register is numeric, return register, if it's
  a symbol, return (reg->idx register)
  If neither, throw IllegalArgumentException"
  [register]
  (if (keyword? register)
    (reg->idx register)
    (if (number? register)
      register
      (throw (IllegalArgumentException. "Register must be either a number or a keyword")))))

(defn reg-get
  [register]
  (let [reg-idx (register-index register)]
    (if (nil? reg-idx)
      (throw (IllegalArgumentException. (str "Register: " register " Doesn't exist")))
      (aget registers reg-idx))))
(defn reg-set
  "Sets the register to value, returns value"
  [register value]
  (let [reg-idx (register-index register)]
    (if (nil? reg-idx)
      (throw (IllegalArgumentException. (str "Register: " register " Doesn't exist")))
      (aset registers reg-idx value))))

;;Program counter
(defonce pc 0)
(defn reset-pc []
  (def pc 0))
(defn set-pc [new-pc]
  (def pc new-pc))
(defn inc-pc []
  (def pc (inc pc)))
;;Stack pointer
(defonce sp 0)

(defn pc-next-word []
  (let [val (aget ram pc)]
    (inc-pc)
    val))

(defn -main [& args]
  (println "dcpu16 emulator/compiler/debugger"))
