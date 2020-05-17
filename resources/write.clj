(def add-nums [
  ; Assign 1 to reg 1
  ;0x 6 1 01
  0x01
  0x61
  ; Assign 2 to reg 2
  ;0x 6 2 02
  0x02
  0x62
  ; Add reg2 into reg1
  ;0x 8 1 2 4
  0x24
  0x81
])

(-> 
  (new java.io.FileOutputStream "test.bin")
  (. write (byte-array add-nums)))
