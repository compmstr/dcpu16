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

;;Registers (A, B, C, X, Y, Z, I, J, EX - overflow, SP, PC, IA - interrupt access)
;;  Again, unsigned words, so use ints
(def registers (int-array 11))
(def register-names [:A :B :C :X :Y :Z :I :J :EX :SP :PC :IA])
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

;;Opcodes are encoded as bbbbbbaaaaaaoooo
;;  2 6-bit values, a evaluated first
;;  4 bit opcode
;;non-basic opcodes are 6-bit value, 6-bit opcode, 4-bit 0's
;;  aaaaaaoooooo0000
;;Values (6 bits)
;;0x00 - 0x07: register (A, B, C, X, Y, Z, I, J) in that order
;;0x08 - 0x0f: [register] -- read memory at loc stored in register
;;0x10 - 0x17: [next word + register]
;;       0x18: POP / [SP++]
;;       0x19: PEEK / [SP]
;;       0x1A: PUSH / [--SP]
;;       0x1B: SP
;;       0x1C: PC
;;       0x1D: EX -- overflow
;;       0x1E: [next word]
;;       0x1F: next word -- literal
;;0x20-0x3f: literal value 0x00-0x1f
(def opcodes
          ;; All take a, b
          [:NON-BASIC ;; 0x0 non-basic instruction
           :SET ;; 0x1 sets a to b
           :ADD ;; 0x2 set a to a+b, sets EX to 0x0001 if overflow, 0x0 otherwise
           :SUB ;; 0x3 set a to a-b, sets EX to 0xFFFF if overflow, 0x0 otherwise
           :MUL ;; 0x4 set a to a*b, sets EX to ((a*b)>>16)&0xFFFF
           :DIV ;; 0x5 set a to a/b, sets EX to ((a<<16)/b)&0xFFFF, if b==0, sets a and EX to 0x0
           :MOD ;; 0x6 sets a to a % b, if b == 0, sets a to 0 instead
           :SHL ;; 0x7 sets a to a<<b, sets EX to ((a<<b)>>16)&0xFFFF
           :SHR ;; 0x8 sets a to a>>b, sets EX to ((a<<16)>>b)&0xFFFF
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
;; ADD [0x1001], EX ; add overflow to high word
;; ADD [0x1001], 0xaabb ; add high words, sets Overflow again

(defn get-next-word
  "Gets the next word starting at PC, and increments PC"
  []
  (let [val (aget ram (reg-get :PC))]
    (inc-pc)
    val))

(defn get-word-op
  [word]
  (num->opcode (bit-and word 0xF)))

;; Values: (5/6 bits)
;;     0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
;;     0x08-0x0f: [register]
;;     0x10-0x17: [next word + register]
;;          0x18: (PUSH/ [--SP]) if in b (POP / [SP++]) if in a
;;          0x19: PEEK / [SP]
;;          0x1a: PICK n / [SP + next word]
;;          0x1b: SP
;;          0x1c: PC
;;          0x1d: EX
;;          0x1e: [next word]
;;          0x1f: next word (literal)
;;     0x20-0x3f: literal value 0x00-0x1f (literal) only for a
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
"mode is either :a or :b, for if you're getting the a value or the b value
"
  [val mode]
    (cond
     (<= val 0x07) ; register
     (register-value val)
     (<= val 0x0f) ; [register]
     (memory-value (reg-get (mod val 0x07)))
     (<= val 0x17) ; [register + next word]
     (memory-value (+ (get-next-word) (reg-get (mod val 0x07))))
     (= val 0x18) ; (PUSH/ [--SP]) if in b (POP / [SP++]) if in a
     (if (= mode :a)
      (let [old-sp (reg-get :SP)]
        (inc-sp)
        (memory-value old-sp))
      (memory-value (dec-sp)))
     (= val 0x19) ; PEEK / [SP]
     (memory-value (reg-get :SP))
     (= val 0x1a) ; PICK n / [SP + next word]
     (memory-value (+ (reg-get :SP) (get-next-word)))
     (= val 0x1b) ; SP
     (register-value :SP)
     (= val 0x1c) ; PC
     (register-value :PC)
     (= val 0x1c) ; O(verflow)
     (register-value :EX)
     (= val 0x1e) ; [next word]
     (memory-value (get-next-word))
     (= val 0x1f) ; next word literal
     (literal-value (get-next-word))
     :else ; literal value 0x00-0x1f(31)
     (if (= mode :a)
      (literal-value (bit-and 0x1f val))
      (println "Invalid value for :b (in-word literal)"))
     ))

(defn get-word-a
  [word]
  (let [raw-a (bit-and 63 (bit-shift-right word 4))]
     (get-value raw-a :a)))
(defn get-word-b
  [word]
  (let [raw-b (bit-and 63 (bit-shift-right word 10))]
    (get-value raw-b :b)))

(defn get-ext-op
  [word]
  (let [ext-op-code (bit-and 63 (bit-shift-right word 4))]
    (num->nonbasic-opcode ext-op-code)))

(defn get-next-code
  []
  (let [next-word (get-next-word)
        op (get-word-op next-word)]
    (if (= next-word 0x0000)
      (do
        (dec-pc)
        nil)
      (if (= op :NON-BASIC)
        {:op op
         :ext-op (get-ext-op next-word)
         :a (get-word-b next-word)}
        {:op op
         :a (get-word-a next-word)
         :b (get-word-b next-word)}))))

(defn skip-next-code
  "Advances CP to the next comman"
  []
  (get-next-code)
  nil)

(defmulti run-nonbasic-word
  (fn [word]
    (:ext-op word)))
(defmethod run-nonbasic-word :JSR
  [word]
  (dec-sp)
  (ram-set (reg-get :SP) (reg-get :PC))
  (reg-set :PC (:val (:a word))))
  

(defmulti run-word
  (fn [word]
    (:op word)))

(defmethod run-word :NON-BASIC
  [word]
  (run-nonbasic-word word))
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
      (reg-set :EX 1)
      (reg-set :EX 0))
    (set-value (:a word) checked-val)))
(defmethod run-word :SUB
  [word]
  (let [new-val (- (:val (:a word))
                   (:val (:b word)))
        checked-val (max new-val 0)
        underflow? (< new-val 0)]
    (if underflow?
      (reg-set :EX 0xFFFF)
      (reg-set :EX 0))
    (set-value (:a word) checked-val)))
(defmethod run-word :MUL
  [word]
  (let [new-val (* (:val (:a word))
                   (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow (bit-and 0xFFFF (bit-shift-right new-val 16))]
    (reg-set :EX overflow)
    (set-value (:a word) checked-val)))
(defmethod run-word :MLI
  [word]
  (let [new-val (* (:val (:a word))
                   (:val (:b word)))
        pos? (> new-val 0)
        checked-val (* (if pos? 1 -1)
                     (bit-and 0x7FFF (Math/abs new-val)))
        overflow (bit-and 0x7FFF (bit-shift-right new-val 15))]
    (reg-set :EX overflow)
    (set-value (:a word) checked-val)))
(defmethod run-word :DIV
  [word]
  (if (= (:val (:b word)) 0)
    (do
      (reg-set :EX 0)
      (set-value (:a word) 0))
    (do
      (let [a-val (:val (:a word))
            b-val (:val (:b word))
            new-val (/ a-val b-val)
            overflow (bit-and 0xFFFF (/ (bit-shift-left a-val 16) b-val))]
        (reg-set :EX overflow)
        (set-value (:a word) new-val)))))
(defmethod run-word :MOD
  [word]
  (if (= (:val (:b word)) 0)
    (set-value (:a word) 0)
    (set-value (:a word) (mod (:val (:a word)) (:val (:b word))))))
(defmethod run-word :SHL
  [word]
  (let [new-val (bit-shift-left (:val (:a word)) (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow (bit-and (bit-shift-right new-val 16) 0xFFFF)]
    (reg-set :EX overflow)
    (set-value (:a word) checked-val)))
(defmethod run-word :SHR
  [word]
  (let [a-val (:val (:a word))
        b-val (:val (:b word))
        new-val (bit-shift-right a-val b-val)
        overflow (bit-and 0xFFFF (bit-shift-right (bit-shift-left a-val 16) b-val))]
    (reg-set :EX overflow)
    (set-value (:a word) new-val)))
(defmethod run-word :AND
  [word]
  (set-value (:a word) (bit-and (:val (:a word)) (:val (:b word)))))
(defmethod run-word :BOR
  [word]
  (set-value (:a word) (bit-or (:val (:a word)) (:val (:b word)))))
(defmethod run-word :XOR
  [word]
  (set-value (:a word) (bit-xor (:val (:a word)) (:val (:b word)))))
(defmethod run-word :IFE
  [word]
  (if (not (= (:val (:a word)) (:val (:b word))))
    (skip-next-code))) 
(defmethod run-word :IFN
  [word]
  (if (= (:val (:a word)) (:val (:b word)))
    (skip-next-code)))
(defmethod run-word :IFG
  [word]
  (if (not (> (:val (:a word)) (:val (:b word))))
    (skip-next-code)))
(defmethod run-word :IFB
  [word]
  (if (= 0 (bit-and (:val (:a word)) (:val (:b word))))
    (skip-next-code)))

(defn -main [& args]
  (println "dcpu16 emulator/compiler/debugger"))

(defn load-test-code
  []
  (let [test-code [;0x7c01 0x0030 ;; SET A, 0x30
                   ;0x7de1 0x1000 0x0020 ;; SET [0x1000], 0x20
                   ;0x7803 0x1000 ;; SUB A, [0x1000]
                   ;0xc00d ;; IFN A, 0x10
                   ;0x7dc1 0x001a ;; SET PC, crash
                   ;Do a loopy thing
                   ;0xa861 ;; loop: SET I 10
                   ;0x7c01 0x2000 ;; SET A, 0x2000
                   ;0x2161 0x2000 ;; SET [0x2000+I], [A]
                   ;0x8463 ;; SUB I, 1
                   ;0x806d ;; IFN I, 0
                   ;0x7dc1 0x000d ;; SET PC, loop
                   ;;Call a subroutine
                   ;0x9031 ;; SET X, 0x4
                   ;0x7c10 0x18 ;; JSR testsub
                   ;0x7dc1 0x001a ;; SET PC, crash
                   ;0x9037 ;; :testsub SHL X, 4
                   ;0x61c1 ;; SET PC, POP
                   ;;0x7dc1 0x001a ;; :crash SET PC, crash
                   ;;Should hang forever now, but X should be 0x40
                   ]]
    (dotimes [i (count test-code)]
      (ram-set i (nth test-code i)))))

(defn test-get
  []
  (reset-pc)
  (load-test-code)
  (take-while #(not (nil? %)) (repeatedly get-next-code)))

(defn print-test
  []
  (doseq [i (test-get)] (println i)))

(defn reset-run
  []
  (reset-pc)
  (reset-sp)
  (load-test-code))
(defn run-step
  []
  (let [next-code (get-next-code)]
    (println "Code:" next-code)
    (if (nil? next-code)
      (println "No more code")
      (run-word next-code))))

(defn run-fast
  []
  (reset-run)
  (loop [next-code (get-next-code)]
    (println "Code:" next-code)
    (if (nil? next-code)
      (println "No more code")
      (do
        (run-word next-code)
        (recur (get-next-code))))))

(defn run-slow
  []
  (reset-run)
  (loop [next-code (get-next-code)]
    (println "Code:" next-code)
    (if (nil? next-code)
      (println "No more code")
      (do
        (run-word next-code)
        (Thread/sleep 250)
        (recur (get-next-code))))))

;;code words:
;;Keys:
;;  :op => op
;;  :a  => a value
;;  :b  => b value
;;  :ext-op => extended op (if needed)
;;Values:
;;  op => keyword
;;  ext-op => keyword
;;  a/b => 
;;    Keys:
;;      :type (one of :register :mem-reg :mem-reg-plus :push/pop
;;             :peek :pick :sp :pc :ex :mem :next-word :literal)
;;      :val value for next word (may be nil)
;;      :register register to use (may be nil)
(defn compile-word
  [word-info])
