(ns chip8.core
  (:gen-class)
  (:require [chip8.disp :refer :all]
            [chip8.vm :refer :all]
            [chip8.opcodes :refer :all]))

(defn load-program! [program] 
  (swap! vm
    update-in [:memory]
    (fn [curr-memory]
      (->>
        (vec program) 
        (concat curr-memory)
        (vec)))))

; TODO: Find a better way to dispatch
(defmulti read-program! 
  #(if (= String (class %)) "path" "data"))

  (defmethod read-program! "path" [path]
    (let [
        program (byte-array (int 1e3)) 
      ]
      (do
        (->
          (new java.io.FileInputStream path)
          (.read program))
        (load-program! program))))

  (defmethod read-program! "data" [data]
    (load-program! data))

(defn execute! [init-state]
  (loop [state init-state]
    (let [
      new-state
        (swap! vm merge
          (let [
            nxt-instr (fetch-nxt-instruction state)
            nxt-opcode (choose-opcode nxt-instr)
            nxt-state 
            (do 
;              (println (read-memory state (state :pc)))
;              (println nxt-instr) 
              (apply (first nxt-opcode) (cons state (rest nxt-opcode))))
          ]
            (do
;              (printf "Instruction: %s\n" (Integer/toHexString nxt-instr))
;              (print nxt-opcode)
;              (print nxt-state)
;              (println "\nDisplay Output:\n")
              ;(printf "%s\r\n" (render-screen nxt-state))
              (update-gui! nxt-state)
              (update-in nxt-state [:pc] + 2))))
    ]
      (do
        (exit-program?)
        (if (> (new-state :pc) (-> (new-state :memory) (count) (- 2)))
          new-state
          (recur @vm))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
    (do
      (start-display!)
      (->>
        (read-program! (first args))
        (execute!))))
