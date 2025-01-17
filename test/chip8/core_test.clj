(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :refer :all]
            [chip8.vm :refer :all]))

(def stock-registers
  (apply sorted-map
    (interleave (range 0x0 0x10) (repeat 0x10 0))))

(deftest core

  (testing "INTEGRATION TEST: random instructions"
    (swap! vm (constantly (new-vm)))
    (as->
      (read-program!
        (byte-array 
          [
            ; Store 1 in reg1
            0x61 0x01 
            ; Increment I-addr by 1
            0xF1 0x1E
            ; Call subroutine A
            0x22 0x32
            ; Store 2 in reg2
            0x62 0x02 
            ; Store 3 in reg3
            0x63 0x03
            ; Add 1 to reg4
            0x74 0x01
            ; Ovewrite reg3 with reg1's value
            0x83 0x10
            ; Add reg2 into reg3
            0x83 0x24

            ; Skips - fail if reg1 has value 255

              ; Skip next instruction since reg1 equals 1
              0x31 0x01
              ; An instruction meant to be skipped (store 255 into reg1)
              0x61 0xFF

              ; Skip next instruction since reg0 equals 0
              0x30 0x00
              ; An instruction meant to be skipped (store 255 into reg1)
              0x61 0xFF

              ; Skip next instruction since reg1 doesn't equal 2
              0x41 0x02
              ; An instruction meant to be skipped (store 255 into reg1)
              0x61 0xFF

              ; Skip next instruction since reg4 doesn't equal 4
              0x44 0x04
              ; An instruction meant to be skipped (store 255 into reg1)
              0x61 0xFF

              ; Don't Skip next instruction since reg1 doesn't equal reg2
              0x51 0x20
              ; An instruction not meant to be skipped (store FF into reg5)
              0x65 0xFF

              ; Don't Skip next instruction since reg4 doesn't equal reg2 (2) 
              0x54 0x20
              ; An instruction not meant to be skipped (store 0 into reg5)
              0x65 0x00
         
              ; Skip next instruction since reg4 doesn't equal reg2 (2) 
              0x94 0x20
              ; An instruction meant to be skipped (store FF into reg5)
              0x65 0xFF

            ; Dump registers to memory
              0xFE 0x55

            ; Then, reload them
              0xFE 0x65

            ; Jump FAR away 
            0x1F 0xFF

              ; Subroutine A
              0x6E 0xFF
              ; End subroutine
              0x00 0xEE
          ]
        )) $
      (execute! $)
      (do
        (is 
          (= (get $ :registers)
            (merge 
              stock-registers 
              {1 1 
               2 2 
               3 3 
               4 1 
               5 0 
               0xE 0xFF})))
        (is
          (= (get $ :pc) 0xFFF))

        (is
          (= (get $ :I-addr) 0x1)))))

  (testing "INTEGRATION TEST: setting I-Addr"
    (let [
      code [0xA2 0xED]
    ]
    (do
      (as->
        (update-in (new-vm) [:memory] #(-> (concat %1 %2) (vec)) code) $
        (execute! $)
        (get $ :I-addr)
        (= 0x2ED $)
        (is $)))))
)
