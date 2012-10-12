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

(defn reverse-map [m]
  (apply hash-map (mapcat reverse m)))

;;Registers (A, B, C, X, Y, Z, I, J, O - overflow, SP, PC)
;;  Again, unsigned words, so use ints
(def registers (int-array 11))
(def register-names [:A :B :C :X :Y :Z :U :J :O :SP :PC])
(def reg->idx (apply hash-map
                     (interleave
                      register-names
                      (range))))
(def idx->reg (reverse-map reg->idx))

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

;;Opcodes are encoded as bbbbbbaaaaaaoooo
;;  2 6-bit values, a evaluated first
;;  4 bit opcode
;;non-basic opcodes are 6-bit value, 6-bit opcode, 4-bit 0's
;;Values (6 bits)
;;0x00 - 0x07: register (A, B, C, X, Y, Z, I, J) in that order
;;0x08 - 0x0f: [register] -- read memory at loc stored in register
;;0x10 - 0x17: [next word + register]
;;       0x18: POP / [SP++]
;;       0x19: PEEK / [SP]
;;       0x1A: PUSH / [--SP]
;;       0x1B: SP
;;       0x1C: PC
;;       0x1D: O -- overflow
;;       0x1E: [next word]
;;       0x1F: next word -- literal
;;0x20-0x3f: literal value 0x00-0x1f
(def opcodes
          ;; All take a, b
          [:NON-BASIC ;; 0x0 non-basic instruction
           :SET ;; 0x1 sets a to b
           :ADD ;; 0x2 set a to a+b, sets O to 0x0001 if overflow, 0x0 otherwise
           :SUB ;; 0x3 set a to a-b, sets O to 0xFFFF if overflow, 0x0 otherwise
           :MUL ;; 0x4 set a to a*b, sets O to ((a*b)>>16)&0xFFFF
           :DIV ;; 0x5 set a to a/b, sets O to ((a<<16)/b)&0xFFFF, if b==0, sets a and O to 0x0
           :MOD ;; 0x6 sets a to a % b, if b == 0, sets a to 0 instead
           :SHL ;; 0x7 sets a to a<<b, sets O to ((a<<b)>>16)&0xFFFF
           :SHR ;; 0x8 sets a to a>>b, sets O to ((a<<16)>>b)&0xFFFF
           :AND ;; 0x9 sets a to a&b
           :BOR ;; 0xa sets a to a|b
           :XOR ;; 0xb sets a to a^b
           :IFE ;; 0xc performs next instruction if a==b
           :IFN ;; 0xd performs next instruction if a!=b
           :IFG ;; 0xe performs next instruction if a>b
           :IFB ;; 0xf performs next instruction if (a&b) != 0
           ])
(def num->opcode
  (apply hash-map (interleave (range) opcodes)))
(def opcode->num
  (reverse-map num->opcode))

(def nonbasic-opcodes
          [:RESERVED
           :JSR ;; a -- pushes address of next instruction on stack, then sets PC to a
          ])
(def num->nonbasic-opcode
  (apply hash-map (interleave (range) nonbasic-opcodes)))
(def nonbasic-opcode->num
  (reverse-map num->nonbasic-opcode))

;; SET, AND, BOR and XOR take 1 cycle, plus the cost of a and b
;; ADD, SUB, MUL, SHR, and SHL take 2 cycles, plus the cost of a and b
;; DIV and MOD take 3 cycles, plus the cost of a and b
;; IFE, IFN, IFG, IFB take 2 cycles, plus the cost of a and b, plus 1 if the test fails
;; JSR takes 2 cycles, plus cost of a

;;JMP -> SET PC, <target>
;;  small jumps ADD PC, <dist> or SUB PC, <dist>
;;RET -> SET PC, POP

;;32-bit add using overflow (add 0x12345678 and 0xaabbccdd)
;; SET [0x1000], 0x5678 ;low word
;; SET [0x1001], 0x1234 ;high word
;; ADD [0x1000], 0xccdd ; add low words, sets Overflow to either 0 or 1
;; ADD [0x1001], O ; add overflow to high word
;; ADD [0x1001], 0xaabb ; add high words, sets Overflow again

;;Program counter
(defn reset-pc []
  (reg-set :PC 0))
(defn set-pc [new-pc]
  (reg-set :PC new-pc))
(defn inc-pc []
  (reg-set :PC (inc (reg-get :PC))))
(defn dec-pc []
  (reg-set :PC (dec (reg-get :PC))))
;;Stack pointer
(defn reset-sp []
  (reg-set :SP 0xffff))
(defn inc-sp []
  (reg-set :SP (inc (reg-get :SP))))
(defn dec-sp []
  (reg-set :SP (dec (reg-get :SP))))

(defn get-next-word
  "Gets the next word starting at PC, and increments PC"
  []
  (let [val (aget ram (reg-get :PC))]
    (inc-pc)
    val))

(defn get-word-op
  [word]
  (num->opcode (bit-and word 0xF)))

;; Values: (6 bits)
;;     0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
;;     0x08-0x0f: [register]
;;     0x10-0x17: [next word + register]
;;          0x18: POP / [SP++]
;;          0x19: PEEK / [SP]
;;          0x1a: PUSH / [--SP]
;;          0x1b: SP
;;          0x1c: PC
;;          0x1d: O
;;          0x1e: [next word]
;;          0x1f: next word (literal)
;;     0x20-0x3f: literal value 0x00-0x1f (literal)
;;     
;; * "next word" really means "[PC++]". These increase the word length of the instruction by 1. 
;; * If any instruction tries to assign a literal value, the assignment fails silently. Other than that, the instruction behaves as normal.
;; * All values that read a word (0x10-0x17, 0x1e, and 0x1f) take 1 cycle to look up. The rest take 0 cycles.
;; * By using 0x18, 0x19, 0x1a as POP, PEEK and PUSH, there's a reverse stack starting at memory location 0xffff. Example: "SET PUSH, 10", "SET X, POP"

(defn memory-value
  "Returns a memory value for get-value"
  [loc]
  {:type :memory
   :loc loc
   :val (ram-get loc)})
(defn register-value
  "Returns a register value for get-value"
  [reg]
  {:type :register
   :register reg
   :register-name (idx->reg reg)
   :val (reg-get reg)})

(defn literal-value
  "Returns a literal value for get-value"
  [val]
  {:type :literal
   :val val})

(defn set-value
  "Sets the value indicated by val (a get-value structure), and sets it to new-val
   returns new-val on success, nil on failure (ex: when literal value is passed in)"
  [dest src]
  (let [new-val (if (map? src)
                  (:val src)
                  src)]
    (case (:type dest)
      :register
      (reg-set (:register dest) new-val)
      :memory
      (ram-set (:loc dest) new-val)
      nil)))

(defn get-value
  [val]
    (cond
     (<= val 0x07) ; register
     (register-value val)
     (<= val 0x0f) ; [register]
     (memory-value (reg-get (mod val 0x07)))
     (<= val 0x17) ; [register + next word]
     (memory-value (+ (get-next-word) (reg-get (mod val 0x07))))
     (= val 0x18) ; POP / [SP++]
     (let [old-sp (reg-get :SP)]
       (inc-sp)
       (memory-value old-sp))
     (= val 0x19) ; PEEK / [SP]
     (memory-value (reg-get :SP))
     (= val 0x1a) ; PUSH / [--SP]
     (memory-value (dec-sp))
     (= val 0x1b) ; SP
     (register-value :SP)
     (= val 0x1c) ; PC
     (register-value :PC)
     (= val 0x1c) ; O(verflow)
     (register-value :O)
     (= val 0x1e) ; [next word]
     (memory-value (get-next-word))
     (= val 0x1f) ; next word literal
     (literal-value (get-next-word))
     :else ; literal value 0x00-0x1f(31)
     (literal-value (bit-and 0x1f val))
     ))

(defn get-word-a
  [word]
  (let [raw-a (bit-and 63 (bit-shift-right word 4))]
     (get-value raw-a)))
(defn get-word-b
  [word]
  (let [raw-b (bit-and 63 (bit-shift-right word 10))]
    (get-value raw-b)))

(defn get-next-code
  []
  (let [next-word (get-next-word)]
    (if (= next-word 0x0000)
      (do
        (dec-pc)
        nil)
      {:op (get-word-op next-word)
       :a (get-word-a next-word)
       :b (get-word-b next-word)})))

(defmulti run-word
  (fn [word] (:op word)))

(defmethod run-word :SET
  [word]
  (set-value (:a word) (:b word)))
(defmethod run-word :ADD
  [word]
  (let [new-val (+ (:val (:a word))
                   (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow? (> new-val checked-val)]
    (if overflow?
      (reg-set :O 1)
      (reg-set :O 0))
    (set-value (:a word) checked-val)))
(defmethod run-word :SUB
  [word]
  (let [new-val (- (:val (:a word))
                   (:val (:b word)))
        checked-val (max new-val 0)
        underflow? (< new-val 0)]
    (if underflow?
      (reg-set :O 0xFFFF)
      (reg-set :O 0))
    (set-value (:a word) checked-val)))
(defmethod run-word :MUL
  [word]
  (let [new-val (* (:val (:a word))
                   (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow (bit-and 0xFFFF (bit-shift-right new-val 16))]
    (reg-set :O overflow)
    (set-value (:a word) checked-val)))
(defmethod run-word :DIV
  [word]
  (set-value (:a word) (/ (:val (:a word)) (:val (:b word)))))
(defmethod run-word :MOD
  [word]
  (set-value (:a word) (mod (:val (:a word)) (:val (:b word)))))

(defn -main [& args]
  (println "dcpu16 emulator/compiler/debugger"))

(defn test-get
  []
  (reset-pc)
  (let [code [0x7c01 0x0030 ;; SET A, 0x30
              0x7de1 0x1000 0x0020 ;; SET [0x1000], 0x20
              0x7803 0x1000 ;; SUB A, [0x1000]
              0xc00d ;; IFN A, 0x10
              0x7dc1 0x001a ;; SET PC, crash
              ]]
    (dotimes [i (count code)]
      (ram-set i (nth code i))))
  (take-while #(not (nil? %)) (repeatedly get-next-code)))

(defn print-test
  []
  (doseq [i (test-get)] (println i)))
