(ns chip8.opcodes-test
  (:require [clojure.test :refer :all]
            [chip8.opcodes :refer :all]
            [chip8.vm :refer :all]))

(def stock-registers
  (apply sorted-map
    (interleave (range 0x0 0x10) (repeat 0x10 0))))

(deftest opcodes
  
  (testing "op6: set register to constant"
    (do
      (->>
        (get-in (op6 (new-vm) 1 0x01) [:registers 1])
        (= 0x01)
        (is))))

  (testing "op7: add constant to register"
    (do
      (->>
        (get-in (op7 (new-vm) 2 0x02) [:registers 2])
        (= 0x02)
        (is))))

  (testing "op8: assign from 1 register to another"
    (let [
      init-state (op6 (new-vm) 2 0xFF)
    ]
      (->>
        (get-in (op8-assign init-state 1 2) [:registers 1])
        (= 0xFF)
        (is))))

  (testing "op8: AND 2 registers into the first one"
    (let [
      init-state (update-in (new-vm) [:registers] merge {1 0xFF 2 0x0F})
    ]
    (do
      (->>
        (get-in (op8-and init-state 1 2) [:registers 1])
        (= 0x0F)
        (is)))))

  (testing "op8: OR 2 registers into the first one"
    (let [
      init-state (update-in (new-vm) [:registers] merge {1 0xFF 2 0x0F})
    ]
    (do
      (->>
        (get-in (op8-or init-state 1 2) [:registers 1])
        (= 0xFF)
        (is)))))

  (testing "op8: XOR 2 registers into the first one"
    (let [
      init-state (update-in (new-vm) [:registers] merge {1 0xFF 2 0x0F})
    ]
    (do
      (->>
        (get-in (op8-xor init-state 1 2) [:registers 1])
        (= 0xF0)
        (is)))))

  (testing "op8: ADD 2 registers"
    (let [
      s1 (update-in (new-vm) [:registers] merge {1 1 2 0})
      s2 (update-in (new-vm) [:registers] merge {1 1 2 1})
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
      s1 (update-in (new-vm) [:registers] merge {1 12 2 8})
      s2 (update-in (new-vm) [:registers] merge {1 8 2 4})
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
      s1 (update-in (new-vm) [:registers] merge {1 8 2 12})
      s2 (update-in (new-vm) [:registers] merge {1 4 2 8})
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
      s (update-in (new-vm) [:registers] merge {1 8 2 9})
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
      s (update-in (new-vm) [:registers] merge {1 6 2 9})
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
      s (update-in (new-vm) [:registers] merge {1 8 2 12})
      overflow (update-in (new-vm) [:registers] merge {1 10 2 128})
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
      s (update-in (new-vm) [:registers] merge {1 12 2 9})
      overflow (update-in (new-vm) [:registers] merge {1 128 2 9})
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