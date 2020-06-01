(ns chip8.vm-test
  (:require [clojure.test :refer :all]
            [chip8.vm :refer :all]))

(deftest tests

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
          (as-> 
            (draw-sprite s 0 0 0xF) $
            (do

              (->
                (get $ :display)
                (= (concat top-display bottom-display))
                (is))

              ; expect VF = 0 since no prev. on pixels were turned off
              (-> 
                (get-in $ [:registers 0xF])
                (= 0)
                (is))))
    
          
          ; Try drawing the same sprite twice - should clear screen
          (as-> 
            (draw-sprite s 0 0 0xF) $
            (draw-sprite $ 0 0 0xF) 
            (do
          
              (->
                (get $ :display)
                (= 
                  (-> 
                    (repeat (* 32 64) 0)
                    (flatten)))
                (is))

              ; Expect VF = 1 since all prev. on pixels were turned off
              (-> 
                (get-in $ [:registers 0xF])
                (= 1)
                (is)))))))
)
