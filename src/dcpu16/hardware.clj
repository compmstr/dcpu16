(ns dcpu16.hardware)

;;Hardware is implemented as plugins
;;Each hardware module needs two functions --
;;  get-info
;;  do-interrupt