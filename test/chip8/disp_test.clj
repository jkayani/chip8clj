(ns chip8.disp-test
  (:require [clojure.test :refer :all]
            [chip8.disp :refer :all]
            [chip8.core :refer :all]))

(def on-pixel "..")
(def off-pixel "  ")

(def off-row (->> (repeat 8 "  ") (reduce str)))
(def on-row (->> (repeat 8 "..") (reduce str)))

(deftest translation

  (testing "number-to-pixel"
    (do
      (-> 
        (number-pixel-row 0x13)
        (=
          (apply str 
            (list
              off-pixel
              off-pixel
              off-pixel
              on-pixel
              off-pixel
              off-pixel
              on-pixel
              on-pixel)))
        (is))
  
      (->
        (number-pixel-row 0)
        (= (apply str (repeat 8 off-pixel)))
        (is))

      (->
        (number-pixel-row 0xFF)
        (= (apply str (repeat 8 on-pixel)))
        (is))))

  (testing "sprite-byte"
    (do

      (->
        (sprite-byte @vm 25 5)
        (= 43)
        (is))

      (->
        (sprite-byte @vm 0 0)
        (= 0)
        (is))

      (->
        (sprite-byte @vm 63 0)
        (= 7)
        (is))

      (->
        (sprite-byte @vm 0 31)
        (= 248)
        (is))

      (->
        (sprite-byte @vm 63 31)
        (= (dec 256))
        (is))))
)

(deftest display

  (testing "rendering a blank screen"
    (print (render-screen @vm)))

)

  
