(def data [
  0xA2 0x0E ; set I to end of code
  0x60 0x00 ; x is 0
  0x61 0x00 ; y is 0
  0xD0 0x1F ; draw
  0x61 0x0F ; y is 15
  0xD0 0x1F ; draw
  0x1F 0xFF ; go FAR away (exit)

  ; sprite (15 full rows)
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
])

(-> 
  (new java.io.FileOutputStream "test.bin")
  (. write (byte-array data)))
