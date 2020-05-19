(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :refer :all]))

(deftest Emulator

  (testing "execution"
    (->> 
      (execute (byte-array [0x61 0x01 0x61 0x02]))
      (= [0x61 0x01 0x61 0x02])
      (is)))
)

(deftest Opcodes
  
  (testing "op6: set register to constant"
    (do
      (op6 1 0x01)
      (->>
        (get-in @vm ["registers" 1])
        (= 0x01)
        (is))))

  ; Note: Determine how to reset! an atom between test runs 
  (testing "op7: add constant to register"
    (do
      (op7 1 0x01)
      (->>
        (get-in @vm ["registers" 1])
        (= 0x02)
        (is))))

  (testing "op8: assign from 1 register to another"
    (do
      (op6 2 0xFF)
      (op8-assign 1 2)
      (->>
        (get-in @vm ["registers" 1])
        (= 0xFF)
        (is))))

  (testing "op8: AND 2 registers into the first one"
    (do
      (op6 2 0xFF)
      (op8-and 1 2)
      (->>
        (get-in @vm ["registers" 1])
        (= 0xFF)
        (is))))

  (testing "op8: OR 2 registers into the first one"
    (do
      (op6 2 0xFF)
      (op8-or 1 2)
      (->>
        (get-in @vm ["registers" 1])
        (= 0xFF)
        (is))))

  (testing "op8: XOR 2 registers into the first one"
    (do
      (op6 2 0xFF)
      (op8-xor 1 2)
      (->>
        (get-in @vm ["registers" 1])
        (= 0xFF)
        (is))))
)
