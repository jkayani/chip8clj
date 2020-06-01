(ns chip8.utils-test
  (:require [clojure.test :refer :all]
            [chip8.utils :refer :all]))

(deftest utils

  (testing "nibbles"
    (do
      (is (= (get-nibble 0x1234 1) 1))
      (is (= (get-nibble 0x1234 2) 2))
      (is (= (get-nibble 0x1234 3) 3))
      (is (= (get-nibble 0x1234 5) nil))
      (is (= (get-nibble 0x1234 0) nil))
      (is (= (get-nibble 0x1234 4) 4))))
)