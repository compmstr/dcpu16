(ns dcpu16.codes)

(def registers
  {:A 0
   :B 1
   :C 2
   :X 3
   :Y 4
   :Z 5
   :I 6
   :J 7})

(def ops
  {:SET 0x01 ;; sets b to a
   :ADD 0x02 ;; set b to b+a, sets EX to 0x0001 if overflow, 0x0 otherwise
   :SUB 0x03 ;; set b to b-a, sets EX to 0xFFFF if overflow, 0x0 otherwise
   :MUL 0x04 ;; set b to b*a, sets EX to ((b*a)>>16)&0xFFFF
   :MLI 0x05 ;; like mul, but a/b are signed
   :DIV 0x06 ;; set b to b/a, sets EX to ((b<<16)/a)&0xFFFF, if a==0, sets b and EX to 0x0
   :DVI 0x07 ;; like div, but b/a are signed
   :MOD 0x08 ;; sets b to b % a, if b == 0, sets b to 0 instead
   :MDI 0x09 ;; like mod, but b,a are signed
   :AND 0x0a ;; sets a to b&a
   :BOR 0x0b ;; sets a to b|a
   :XOR 0x0c ;; sets a to b^a
   :SHR 0x0d ;; sets a to b>>a, sets EX to ((b<<16)>>a)&0xFFFF
   :ASR 0x0e ;; signed b>>a
   :SHL 0x0f ;; sets a to b<<a, sets EX to ((b<<a)>>16)&0xFFFF
   :IFB 0x10 ;; performs next instruction if (b&a) != 0
   :IFC 0x11 ;; performs next instruction if (b&a)==0
   :IFE 0x12 ;; performs next instruction if b==a
   :IFN 0x13 ;; performs next instruction if b!=a
   :IFG 0x14 ;; performs next instruction if b>a
   :IFA 0x15 ;; performs next instruction if b>a (signed)
   :IFL 0x16 ;; performs next instruction if b<a
   :IFU 0x17 ;; performs next instruction if b<a (signed)
   :ADX 0x1a ;; sets b to b+a+EX, sets EX to 0x0001 if overflow, 0x0 otherwise
   :SBX 0x1b ;; set b to b-a+EX, sets EX to 0xFFFF if underflow, 0x0 otherwise
   :STI 0x1e ;; sets b to a, then increase I and J by 1
   :STD 0x1f ;; sets b to a, then decrease I and J by 1
   })

(def special-ops
  {:JSR 0x01 ;; a - pushes address of next instruction on stack, then sets PC to a
   :INT 0x08 ;; - triggers software interrupt with message a
   :IAG 0x09 ;; - sets a to IA
   :IAS 0x0a ;; - Sets IA to a
   :RFI 0x0b ;; - disables interrupt queueing, pops A from stack, then
             ;;         pops PC from stack
   :IAQ 0x0c ;; - if a is nonzero, interrupts will be added to the queue instead
             ;;         of triggered, if zero, interrupts will be triggered as normal again
   :HWN 0x10 ;; - sets a to number of connected HW devices
             ;;        A+(B<<16) is 32 bit dword Hardware ID
             ;;        C is hardware version
             ;;        X+(Y<<16) is 32 bit dword with manufacturer ID
   :HWQ 0x11 ;; - sets A/B/C/X/Y registers to info about HW a
   :HWI 0x12 ;; - sends interrupt to HW a
   })