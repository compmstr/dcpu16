(ns dcpu16.vm
  (:import [dcpu16 Vm VmLoc])
  (:use dcpu16.util))

;;For REPL, hide array elements after 100
(set! *print-length* 100)

(defn create-vm
  []
  {:cycles 0
;;0x10000 words of ram
;;  Each word is unsigned, so using ints
   :ram (apply vector (take 0x10000 (repeat 0)))
   :registers (apply vector (take 8 (repeat 0)))
   :EX 0 :SP 0 :PC 0 :IA 0})

(def register-list (zipmap [:A :B :C :I :J :X :Y :Z] (range 8)))
;;(defonce vm (Vm.))
(defonce vm (create-vm))

(defn reg-set
  [reg val]
  (dosync
   (ref-set vm
            (assoc @vm :registers
              (if (keyword? reg)
                (assoc (:registers @vm) (register-list reg) val)
                (assoc (:registers @vm) reg val))))))
(defn reg-get
  [reg]
  (if (keyword? reg)
    (nth (:registers @vm) (register-list reg))
    (nth (:registers @vm) reg)))

(defn ram-set
  [loc val]
  (dosync
   (ref-set vm
            (assoc @vm :ram (assoc (:ram @vm) loc (bit-and 0xFFFFFF val))))))
(defn ram-get
  [loc]
  (nth (:ram @vm) loc))

(defmacro vm-field
  "Create a get/set/inc/dec/reset field for the vm
   passing in pc for example will create get-pc, set-pc, reset-pc, etc"
  [name]
  `(do (defn ~(symbol (str "set-" name))
         [val#]
         (dosync
          (ref-set vm
                   (assoc @vm (keyword (quote ~name)) val#))))
       (defn ~(symbol (str "get-" name))
         []
         ((keyword (quote ~name)) @vm))
       (defn ~(symbol (str "reset-" name))
         []
         (~(symbol (str "set-" name)) 0))
       (defn ~(symbol (str "inc-" name))
         []
         (~(symbol (str "set-" name))
          (inc (~(symbol (str "get-" name))))))
       (defn ~(symbol (str "dec-" name))
         []
         (~(symbol (str "set-" name))
          (dec (~(symbol (str "get-" name))))))))

;;Program counter
(vm-field PC)
;;Interrupt Address (IA)
(vm-field IA)
;;Overflow
(vm-field EX)
;;Stack pointer
(vm-field SP)

;;Opcodes are encoded as aaaaaabbbbbooooo
;;  6-bit value, a
;;  5-bit value, b
;;  5 bit opcode
;;special opcodes are 6-bit value, 6-bit opcode, 4-bit 0's
;;  aaaaaaoooooo0000
;;C is time in cycles to look up value or perform opcode
;;--- Values: (5/6 bits) ---------------------------------------------------------
;; C | VALUE     | DESCRIPTION
;;---+-----------+----------------------------------------------------------------
;; 0 | 0x00-0x07 | register (A, B, C, X, Y, Z, I or J, in that order)
;; 0 | 0x08-0x0f | [register]
;; 1 | 0x10-0x17 | [register + next word]
;; 0 |      0x18 | (PUSH / [--SP]) if in b, or (POP / [SP++]) if in a
;; 0 |      0x19 | [SP] / PEEK
;; 1 |      0x1a | [SP + next word] / PICK n
;; 0 |      0x1b | SP
;; 0 |      0x1c | PC
;; 0 |      0x1d | EX
;; 1 |      0x1e | [next word]
;; 1 |      0x1f | next word (literal)
;; 0 | 0x20-0x3f | literal value 0xffff-0x1e (-1..30) (literal) (only for a)
;; --+-----------+----------------------------------------------------------------
(def opcodes
          ;; All take b, a
          [:SPECIAL ;; 0x0 special instruction
           :SET ;; 0x01 sets b to a
           :ADD ;; 0x02 set b to b+a, sets EX to 0x0001 if overflow, 0x0 otherwise
           :SUB ;; 0x03 set b to b-a, sets EX to 0xFFFF if overflow, 0x0 otherwise
           :MUL ;; 0x04 set b to b*a, sets EX to ((b*a)>>16)&0xFFFF
           :MLI ;; 0x05 like mul, but a/b are signed
           :DIV ;; 0x06 set b to b/a, sets EX to ((b<<16)/a)&0xFFFF, if a==0, sets b and EX to 0x0
           :DVI ;; 0x07 like div, but b/a are signed
           :MOD ;; 0x08 sets b to b % a, if b == 0, sets b to 0 instead
           :MDI ;; 0x09 like mod, but b,a are signed
           :AND ;; 0x0a sets a to b&a
           :BOR ;; 0x0b sets a to b|a
           :XOR ;; 0x0c sets a to b^a
           :SHR ;; 0x0d sets a to b>>a, sets EX to ((b<<16)>>a)&0xFFFF
           :ASR ;; 0x0e signed b>>a
           :SHL ;; 0x0f sets a to b<<a, sets EX to ((b<<a)>>16)&0xFFFF
           :IFB ;; 0x10 performs next instruction if (b&a) != 0
           :IFC ;; 0x11 performs next instruction if (b&a)==0
           :IFE ;; 0x12 performs next instruction if b==a
           :IFN ;; 0x13 performs next instruction if b!=a
           :IFG ;; 0x14 performs next instruction if b>a
           :IFA ;; 0x15 performs next instruction if b>a (signed)
           :IFL ;; 0x16 performs next instruction if b<a
           :IFU ;; 0x17 performs next instruction if b<a (signed)
           :R18 ;; 0x18 -reserved-
           :R19 ;; 0x19 -reserved-
           :ADX ;; 0x1a sets b to b+a+EX, sets EX to 0x0001 if overflow, 0x0 otherwise
           :SBX ;; 0x1b set b to b-a+EX, sets EX to 0xFFFF if underflow, 0x0 otherwise
           :R1C ;; 0x1c -reserved-
           :R1D ;; 0x1d -reserved-
           :STI ;; 0x1e sets b to a, then increase I and J by 1
           :STD ;; 0x1f sets b to a, then decrease I and J by 1
           ])
(def num->opcode
  (apply hash-map (interleave (range) opcodes)))
(def opcode->num
  (reverse-map num->opcode))

;;Special opcodes always have their lower five bits unset, have one value and a
;;five bit opcode. In binary, they have the format: aaaaaaooooo00000
;;The value (a) is in the same six bit format as defined earlier.
;;
;;--- Special opcodes: (5 bits) --------------------------------------------------
;; C | VAL  | NAME  | DESCRIPTION
;;---+------+-------+-------------------------------------------------------------
;; - | 0x00 | n/a   | reserved for future expansion
;; 3 | 0x01 | JSR a | pushes the address of the next instruction to the stack,
;;   |      |       | then sets PC to a
;; - | 0x02 | -     |
;; - | 0x03 | -     |
;; - | 0x04 | -     |
;; - | 0x05 | -     |
;; - | 0x06 | -     |
;; - | 0x07 | -     |
;; 4 | 0x08 | INT a | triggers a software interrupt with message a
;; 1 | 0x09 | IAG a | sets a to IA
;; 1 | 0x0a | IAS a | sets IA to a
;; 3 | 0x0b | RFI a | disables interrupt queueing, pops A from the stack, then
;;   |      |       | pops PC from the stack
;; 2 | 0x0c | IAQ a | if a is nonzero, interrupts will be added to the queue
;;   |      |       | instead of triggered. if a is zero, interrupts will be
;;   |      |       | triggered as normal again
;; - | 0x0d | -     |
;; - | 0x0e | -     |
;; - | 0x0f | -     |
;; 2 | 0x10 | HWN a | sets a to number of connected hardware devices
;; 4 | 0x11 | HWQ a | sets A, B, C, X, Y registers to information about hardware a
;;   |      |       | A+(B<<16) is a 32 bit word identifying the hardware id
;;   |      |       | C is the hardware version
;;   |      |       | X+(Y<<16) is a 32 bit word identifying the manufacturer
;; 4+| 0x12 | HWI a | sends an interrupt to hardware a
;; - | 0x13 | -     |
;; - | 0x14 | -     |
;; - | 0x15 | -     |
;; - | 0x16 | -     |
;; - | 0x17 | -     |
;; - | 0x18 | -     |
;; - | 0x19 | -     |
;; - | 0x1a | -     |
;; - | 0x1b | -     |
;; - | 0x1c | -     |
;; - | 0x1d | -     |
;; - | 0x1e | -     |
;; - | 0x1f | -     |
;;---+------+-------+-------------------------------------------------------------

(def special-opcodes
          [:RESERVED
           :JSR ;; a - pushes address of next instruction on stack, then sets PC to a
           :R02
           :R03
           :R04
           :R05
           :R06
           :R07
           :INT ;; 0x08 - triggers software interrupt with message a
           :IAG ;; 0x09 - sets a to IA
           :IAS ;; 0x0a - Sets IA to a
           :RFI ;; 0x0b - disables interrupt queueing, pops A from stack, then
                ;;         pops PC from stack
           :IAQ ;; 0x0c - if a is nonzero, interrupts will be added to the queue instead
                ;;         of triggered, if zero, interrupts will be triggered as normal again
           :R0D
           :R0E
           :R0F
           :HWN ;; 0x10 - sets a to number of connected HW devices
                ;;        A+(B<<16) is 32 bit dword Hardware ID
                ;;        C is hardware version
                ;;        X+(Y<<16) is 32 bit dword with manufacturer ID
           :HWQ ;; 0x11 - sets A/B/C/X/Y registers to info about HW a
           :HWI ;; 0x12 - sends interrupt to HW a
          ])
(def num->special-opcode
  (doall (apply hash-map (interleave (range) special-opcodes))))
(def special-opcode->num
  (doall (reverse-map num->special-opcode)))

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
  (let [val (ram-get (get-PC))]
    (inc-PC)
    val))

(defn get-word-op
  "Get operand from lowest 5 bits of word"
  [word]
  (bit-and word 0x1F))

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
  ;;(println "Dest:" dest " -- Src:" src)
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
     (memory-value (reg-get (mod val 0x08)))
     (<= val 0x17) ; [register + next word]
     (memory-value (+ (get-next-word) (reg-get (mod val 0x08))))
     (= val 0x18) ; (PUSH/ [--SP]) if in b (POP / [SP++]) if in a
     (if (= mode :a)
      (let [old-sp (get-SP)]
        (inc-SP)
        (memory-value old-sp))
      (memory-value (dec-SP)))
     (= val 0x19) ; PEEK / [SP]
     (memory-value (get-SP))
     (= val 0x1a) ; PICK n / [SP + next word]
     (memory-value (+ (get-SP) (get-next-word)))
     (= val 0x1b) ; SP
     (get-SP)
     (= val 0x1c) ; PC
     (get-PC)
     (= val 0x1c) ; O(verflow)
     (get-EX)
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
  "a-value for word, highest 6 bits"
  [word]
  (let [raw-a (bit-and 2r111111 (bit-shift-right word 10))]
     (get-value raw-a :a)))
(defn get-word-b
  "b-value for word, highest 5 bits after first 6"
  [word]
  (let [raw-b (bit-and 2r11111 (bit-shift-right word 5))]
    (get-value raw-b :b)))

(defn get-ext-op
  [word]
  (let [ext-op-code (bit-and 63 (bit-shift-right word 4))]
    (num->special-opcode ext-op-code)))

(defn get-next-code
  []
  (let [next-word (get-next-word)
        opcode (get-word-op next-word)
        op (num->opcode opcode)]
    (if (= next-word 0x0000)
      (do
        (dec-PC)
        nil)
      (if (= op :SPECIAL)
        {:op op
         :opcode opcode
         :ext-op (get-ext-op next-word)
         :a (get-word-b next-word)}
        {:op op
         :opcode opcode
         :a (get-word-a next-word)
         :b (get-word-b next-word)}))))

(defn skip-next-code
  "Advances CP to the next command"
  []
  (get-next-code)
  nil)

(defmulti run-special-op
  (fn [word]
    (:ext-op word)))
(defmethod run-special-op :JSR
  [word]
  (dec-SP)
  (ram-set (get-SP) (get-PC))
  (set-PC (:val (:a word))))

(defn run-op_SPECIAL
  [word]
  (run-special-op word))
(defn run-op_SET
  [word]
  (println "op-SET")
  (set-value (:b word) (:a word)))
(defn run-op_ADD
  [word]
  (println "op-ADD")
  (let [new-val (+ (:val (:a word))
                   (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow? (> new-val checked-val)]
    (if overflow?
      (set-EX 1)
      (set-EX 0))
    (set-value (:b word) checked-val)))
(defn run-op_SUB
  [word]
  (println "op-SUB")
  (let [new-val (- (:val (:b word))
                   (:val (:a word)))
        checked-val (max new-val 0)
        underflow? (< new-val 0)]
    (if underflow?
      (set-EX 0xFFFF)
      (set-EX 0))
    (set-value (:b word) checked-val)))
(defn run-op_MUL
  [word]
  (let [new-val (* (:val (:a word))
                   (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow (bit-and 0xFFFF (bit-shift-right new-val 16))]
    (set-EX overflow)
    (set-value (:a word) checked-val)))
(defn run-op_MLI
  [word]
  (let [new-val (* (:val (:a word))
                   (:val (:b word)))
        pos? (> new-val 0)
        checked-val (* (if pos? 1 -1)
                     (bit-and 0x7FFF (Math/abs ^Integer new-val)))
        overflow (bit-and 0x7FFF (bit-shift-right new-val 15))]
    (set-EX overflow)
    (set-value (:a word) checked-val)))
(defn run-op_DIV
  [word]
  (if (= (:val (:b word)) 0)
    (do
      (set-EX 0)
      (set-value (:a word) 0))
    (do
      (let [a-val (:val (:a word))
            b-val (:val (:b word))
            new-val (/ a-val b-val)
            overflow (bit-and 0xFFFF (/ (bit-shift-left a-val 16) b-val))]
        (set-EX overflow)
        (set-value (:a word) new-val)))))
(defn run-op_DVI
  [word]
  (if (= (:val (:b word)) 0)
    (do
      (set-EX 0)
      (set-value (:a word) 0))
    (do
      (let [a-val (:val (:a word))
            b-val (:val (:b word))
            new-val (/ a-val b-val)
            pos? (> new-val 0)
            checked-val (* (if pos? 1 -1)
                           (bit-and 0x7FFF new-val))
            overflow (bit-and 0xFFFF (/ (bit-shift-left a-val 15) b-val))]
        (set-EX overflow)
        (set-value (:a word) checked-val)))))
(defn run-op_MOD
  [word]
  (if (= (:val (:b word)) 0)
    (set-value (:a word) 0)
    (set-value (:a word) (mod (:val (:a word)) (:val (:b word))))))
(defn run-op_MDI
  [word])
(defn run-op_SHL
  [word]
  (let [new-val (bit-shift-left (:val (:a word)) (:val (:b word)))
        checked-val (bit-and 0xFFFF new-val)
        overflow (bit-and (bit-shift-right new-val 16) 0xFFFF)]
    (set-EX overflow)
    (set-value (:a word) checked-val)))
(defn run-op_SHR
  [word]
  (let [a-val (:val (:a word))
        b-val (:val (:b word))
        new-val (bit-shift-right a-val b-val)
        overflow (bit-and 0xFFFF (bit-shift-right (bit-shift-left a-val 16) b-val))]
    (set-EX overflow)
    (set-value (:a word) new-val)))
(defn run-op_ASR
  [word]
  )
(defn run-op_AND
  [word]
  (set-value (:a word) (bit-and (:val (:a word)) (:val (:b word)))))
(defn run-op_BOR
  [word]
  (set-value (:a word) (bit-or (:val (:a word)) (:val (:b word)))))
(defn run-op_XOR
  [word]
  (set-value (:a word) (bit-xor (:val (:a word)) (:val (:b word)))))
(defn run-op_IFC
  [word])
(defn run-op_IFE
  [word]
  (if (not (= (:val (:a word)) (:val (:b word))))
    (skip-next-code)))
(defn run-op_IFN
  [word]
  (println "op-IFN")
  (if (= (:val (:a word)) (:val (:b word)))
    (skip-next-code)))
(defn run-op_IFG
  [word]
  (if (not (> (:val (:a word)) (:val (:b word))))
    (skip-next-code)))
(defn run-op_IFA
  [word])
(defn run-op_IFL
  [word])
(defn run-op_IFU
  [word])
(defn run-op_IFB
  [word]
  (if (= 0 (bit-and (:val (:a word)) (:val (:b word))))
    (skip-next-code)))
(defn run-op_ADX
  [word]
  )
(defn run-op_SBX
  [word]
  )
(defn run-op_STI
  [word]
  )
(defn run-op_STD
  [word]
  )
(defn run-op_default
  [word]
  (println "Invalid word operation:" word))

(def run-op-map
  {
   (opcode->num :SET) run-op_SET 
   (opcode->num :ADD) run-op_ADD 
   (opcode->num :SUB) run-op_SUB 
   (opcode->num :MUL) run-op_MUL 
   (opcode->num :MLI) run-op_MLI 
   (opcode->num :DIV) run-op_DIV 
   (opcode->num :DVI) run-op_DVI 
   (opcode->num :MOD) run-op_MOD 
   (opcode->num :MDI) run-op_MDI 
   (opcode->num :AND) run-op_AND 
   (opcode->num :BOR) run-op_BOR 
   (opcode->num :XOR) run-op_XOR 
   (opcode->num :SHR) run-op_SHR 
   (opcode->num :ASR) run-op_ASR 
   (opcode->num :SHL) run-op_SHL 
   (opcode->num :IFB) run-op_IFB 
   (opcode->num :IFC) run-op_IFC 
   (opcode->num :IFE) run-op_IFE 
   (opcode->num :IFN) run-op_IFN 
   (opcode->num :IFG) run-op_IFG 
   (opcode->num :IFA) run-op_IFA 
   (opcode->num :IFL) run-op_IFL 
   (opcode->num :IFU) run-op_IFU 
   (opcode->num :ADX) run-op_ADX 
   (opcode->num :SBX) run-op_SBX 
   (opcode->num :STI) run-op_STI 
   (opcode->num :STD) run-op_STD 
   })

(defn run-op
  [word]
  ((run-op-map (:opcode word)) word))

(defn load-test-code
  []
  (let [test-code [0x7c01 0x0030 ;; SET A, 0x30
                   0x7fc1 0x0020 0x1000 ;; SET [0x1000], 0x20
                   0x7803 0x1000 ;; SUB A, 0x10
                   0xc013 ;; IFN A, 10
                   0x7f81 0x0020 ;; SET PC, end(0x20)
                   0xa8c1 ;; SET I, 10
                   0x7C01 0x2000 ;; SET A, 0x2000
                   ;;:loop
                   0x22c1 0x2000 ;; SET [0x2000+I], [A]
                   0x84c3 ;; SUB I, 1
                   0x80d3 ;; IFN I, 0
                   0xb781 ;; SET PC, loop
                   ]]
    (dotimes [i (count test-code)]
      (ram-set i (nth test-code i)))))


(defn reset-run
  []
  (reset-PC)
  (reset-SP)
  (load-test-code))
(defn run-step
  "Runs a single step of the vm, returns nil when no more code is available"
  ([]
     (run-step {}))
  ([args]
     (let [next-code (get-next-code)]
       (if (= (:debug args) :true)
        (println "Code:" next-code))
       (if (nil? next-code)
         (do
           (println "No more code")
           nil)
         (do
           (run-op next-code)
           1)))))

(defn run-fast
  []
  (reset-run)
  (while (run-step)))
(defn debug-run-fast
  []
  (reset-run)
  (while (run-step {:debug :true})))

(defn run-slow
  []
  (reset-run)
  (while (run-step)
    (Thread/sleep 250)))
(defn debug-run-slow
  []
  (reset-run)
  (while (run-step {:debug :true})
    (Thread/sleep 250)))

