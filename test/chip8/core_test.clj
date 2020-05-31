(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :refer :all]))

(def stock-registers
  (apply sorted-map
    (interleave (range 0x0 0x10) (repeat 0x10 0))))

(deftest bitshift

  (testing "nibbles"
    (do
      (is (= (get-nibble 0x1234 1) 1))
      (is (= (get-nibble 0x1234 2) 2))
      (is (= (get-nibble 0x1234 3) 3))
      (is (= (get-nibble 0x1234 5) nil))
      (is (= (get-nibble 0x1234 0) nil))
      (is (= (get-nibble 0x1234 4) 4))))
)

(deftest REPL

  (testing "INTEGRATION TEST: random instructions"
    (swap! vm (constantly (new-vm)))
    (as->
      (read-program!
        (byte-array 
          [
            ; Store 1 in reg1
            0x61 0x01 
            ; Call subroutine A
            0x20 0x2C
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
          (= (get $ :pc) 0xFFF)))))

)

(deftest displayIO

  (testing "setting I-Addr"
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

  (testing "opcode: draw-sprite"
    (let [
      sprite-data (repeat 0xF 0xFF)
      s (->
          (assoc (new-vm) :I-addr 0x0200)
          (update-in [:memory] #(-> (concat %1 %2) (vec)) sprite-data))
      top-display
        (->
          (repeat
            15
            (concat (repeat 8 1) (repeat (- 64 8) 0)))
          (flatten))
      ; 15 screen rows will be drawn on, so 17 should be untouched
      bottom-display
        (repeat (* 17 64) 0)
    ]
      (do 
          ; Draw sprite onto blank screen
          (-> 
            (draw-sprite s 0 0 0xF) 
            (get :display)
            (= (concat top-display bottom-display))
            (is))

          ; Draw sprite onto blank screen, 
          ; expect VF = 0 since no prev. on pixels were turned off
          (-> 
            (draw-sprite s 0 0 0xF) 
            (get-in [:registers 0xF])
            (= 0)
            (is))
    
          
          ; Try drawing the same sprite twice - should clear screen
          (-> 
            (draw-sprite s 0 0 0xF) 
            (draw-sprite 0 0 0xF) 
            (get :display)
            (= 
              (-> 
                (repeat (* 32 64) 0)
                (flatten)))
            (is))

          ; Expect VF = 1 since all prev. on pixels were turned off
          (-> 
            (draw-sprite s 0 0 0xF) 
            (draw-sprite 0 0 0xF) 
            (get-in [:registers 0xF])
            (= 1)
            (is)))))
)

(deftest Opcodes
  
  (testing "op6: set register to constant"
    (do
      (->>
        (get-in (op6 @vm 1 0x01) [:registers 1])
        (= 0x01)
        (is))))

  ; Note: Determine how to reset! an atom between test runs 
  (testing "op7: add constant to register"
    (do
      (->>
        (get-in (op7 @vm 2 0x02) [:registers 2])
        (= 0x02)
        (is))))

  (testing "op8: assign from 1 register to another"
    (let [
      init-state (op6 @vm 2 0xFF)
    ]
      (->>
        (get-in (op8-assign init-state 1 2) [:registers 1])
        (= 0xFF)
        (is))))

  (testing "op8: AND 2 registers into the first one"
    (let [
      init-state (update-in @vm [:registers] merge {1 0xFF 2 0x0F})
    ]
    (do
      (->>
        (get-in (op8-and init-state 1 2) [:registers 1])
        (= 0x0F)
        (is)))))

  (testing "op8: OR 2 registers into the first one"
    (let [
      init-state (update-in @vm [:registers] merge {1 0xFF 2 0x0F})
    ]
    (do
      (->>
        (get-in (op8-or init-state 1 2) [:registers 1])
        (= 0xFF)
        (is)))))

  (testing "op8: XOR 2 registers into the first one"
    (let [
      init-state (update-in @vm [:registers] merge {1 0xFF 2 0x0F})
    ]
    (do
      (->>
        (get-in (op8-xor init-state 1 2) [:registers 1])
        (= 0xF0)
        (is)))))

  (testing "op8: ADD 2 registers"
    (let [
      s1 (update-in @vm [:registers] merge {1 1 2 0})
      s2 (update-in @vm [:registers] merge {1 1 2 1})
    ]
    (do
      ; No carry flag
      (as->
        (get-in (op8-add s1 1 2) [:registers]) $
        (and (= 1 ($ 1)) (= 0 ($ 0xF)))
        (is $))
      ; carry flag
      (as->
        (get-in (op8-add s2 1 2) [:registers]) $
        (and (= 2 ($ 1)) (= 1 ($ 0xF)))
        (is $)))))

  (testing "op8: SUBTRACT 2 registers"
    (let [
      s1 (update-in @vm [:registers] merge {1 12 2 8})
      s2 (update-in @vm [:registers] merge {1 8 2 4})
    ]
    (do
      ; No borrow 
      (as->
        (get-in (op8-subtract s1 1 2) [:registers]) $
        (and (= 4 ($ 1)) (= 1 ($ 0xF)))
        (is $))
      ; borrow flag
      (as->
        (get-in (op8-subtract s2 1 2) [:registers]) $
        (and (= 4 ($ 1)) (= 0 ($ 0xF)))
        (is $)))))

  (testing "op8: SUBTRACT 2 registers - FLIPPED"
    (let [
      s1 (update-in @vm [:registers] merge {1 8 2 12})
      s2 (update-in @vm [:registers] merge {1 4 2 8})
    ]
    (do
      ; No borrow 
      (as->
        (get-in (op8-subtract-flipped s1 1 2) [:registers]) $
        (and (= 4 ($ 1)) (= 1 ($ 0xF)))
        (is $))
      ; borrow flag
      (as->
        (get-in (op8-subtract-flipped s2 1 2) [:registers]) $
        (and (= 4 ($ 1)) (= 0 ($ 0xF)))
        (is $)))))

  (testing "op8: BITSHIFT RIGHT (CHIP8)"
    (let [
      s (update-in @vm [:registers] merge {1 8 2 9})
    ]
    (do
      (as->
        (get-in (op8-shift-right-std s 1 2) [:registers]) $
        (and 
          (= 4 ($ 1)) 
          (= 9 ($ 2))
          (= 1 ($ 0xF)))
        (is $)))))

  (testing "op8: BITSHIFT RIGHT (SCHIP)"
    (let [
      s (update-in @vm [:registers] merge {1 6 2 9})
    ]
    (do
      (as->
        (get-in (op8-shift-right-schip s 1 2) [:registers]) $
        (and 
          (= 3 ($ 1)) 
          (= 9 ($ 2))
          (= 0 ($ 0xF)))
        (is $)))))

  (testing "op8: BITSHIFT LEFT (CHIP8)"
    (let [
      s (update-in @vm [:registers] merge {1 8 2 12})
      overflow (update-in @vm [:registers] merge {1 10 2 128})
    ]
    (do
      (as->
        (get-in (op8-shift-left-std s 1 2) [:registers]) $
        (and 
          (= 24 ($ 1)) 
          (= 12 ($ 2))
          (= 0 ($ 0xF)))
        (is $))
      (as->
        (get-in (op8-shift-left-std overflow 1 2) [:registers]) $
        (and 
          (= 0 ($ 1)) 
          (= 128 ($ 2))
          (= 1 ($ 0xF)))
        (is $)))))

  (testing "op8: BITSHIFT LEFT (SCHIP)"
    (let [
      s (update-in @vm [:registers] merge {1 12 2 9})
      overflow (update-in @vm [:registers] merge {1 128 2 9})
    ]
    (do
      (as->
        (get-in (op8-shift-left-schip s 1 2) [:registers]) $
        (and 
          (= 24 ($ 1)) 
          (= 9 ($ 2))
          (= 0 ($ 0xF)))
        (is $))
      (as->
        (get-in (op8-shift-left-schip overflow 1 2) [:registers]) $
        (and 
          (= 0 ($ 1)) 
          (= 9 ($ 2))
          (= 1 ($ 0xF)))
        (is $)))))
)
