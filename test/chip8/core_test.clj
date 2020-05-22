(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :refer :all]))

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
    (->>
      (read-program!
        (byte-array [0x61 01 0x62 02 0x63 03]))
      (execute!)))
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
      (op6 1 0x01)
      (->>
        (get-in @vm [:registers 1])
        (= 0x01)
        (is))))

  ; Note: Determine how to reset! an atom between test runs 
  (testing "op7: add constant to register"
    (do
      (op7 1 0x01)
      (->>
        (get-in @vm [:registers 1])
        (= 0x02)
        (is))))

  (testing "op8: assign from 1 register to another"
    (do
      (op6 2 0xFF)
      (op8-assign 1 2)
      (->>
        (get-in @vm [:registers 1])
        (= 0xFF)
        (is))))

  (testing "op8: AND 2 registers into the first one"
    (do
      (op6 2 0xFF)
      (op8-and 1 2)
      (->>
        (get-in @vm [:registers 1])
        (= 0xFF)
        (is))))

  (testing "op8: OR 2 registers into the first one"
    (do
      (op6 2 0xFF)
      (op8-or 1 2)
      (->>
        (get-in @vm [:registers 1])
        (= 0xFF)
        (is))))

  (testing "op8: XOR 2 registers into the first one"
    (do
      (op6 2 0xFF)
      (op8-xor 1 2)
      (->>
        (get-in @vm [:registers 1])
        (= 0xFF)
        (is))))
)
