(ns dcpu16.core
  (:import [dcpu16 Vm VmLoc])
  (:require [dcpu16.vm :as vm]
            [dcpu16.compiler :as compiler])
  (:use dcpu16.util))


(defn -main [& args]
  (println "dcpu16 emulator/compiler/debugger"))

(defn test-encode
  []
  (let [test-code ["SET A, 0x30", "SET [0x1000], 0x20", "SUB A, [0x1000]",
                   "IFN A, 0x10"]]
    (map #(println
           (map hex-string
                (dcpu16.encoder/encode
                 (first (:code-entries (dcpu16.parser/process-line
                                        %
                                        {:pos 0 :code-entries []})))))) test-code)))

(defn test-vm
  []
  (def vm (Vm.))
  (def aval (VmLoc. vm))
  (def bval (VmLoc. vm)))

(defn test-get
  []
  (vm/reset-PC)
  (vm/load-test-code)
  (take-while #(not (nil? %)) (repeatedly vm/get-next-code)))

(defn print-test
  []
  (doseq [i (test-get)] (println i)))

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

(defn compile-word-chunks
  [a b op]
  (bit-or
   (bit-shift-left a 10)
   (bit-shift-left b 5)
   op))

(defn pcwc
  [a b op]
  (format "0x%X" (compile-word-chunks a b op)))

(defn as-hex
  [num]
  (format "0x%X" num))
