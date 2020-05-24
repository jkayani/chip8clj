(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :refer :all]))

(def stock-registers (zipmap (range 0 0x10) (repeat 0xF 0)))

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

  (testing "repl"
    (->
      (read-program!
        (byte-array [0x61 1 0x62 2 0x63 3]))
      (execute!)
      (get :registers)
      (= 
        (merge 
          stock-registers 
          {1 1 2 2 3 3}))
      (is)))

)

(deftest IO 

  (testing "read program data"
    (->> 
      ((read-program! (byte-array [0x61 0x01 0x61 0x02])) :memory)
      (= [0x61 0x01 0x61 0x02])
      (is)))
)

(deftest OpcodeSelection
  
  (testing "op6"
    (->> 
      (choose-opcode 0x6101)
      (= (list op6 1 1))
      (is)))
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
)
