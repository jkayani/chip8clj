(ns chip8.core
  (:gen-class))

(def stack-frame nil)

(def vm (atom {
  :memory []
  :registers (zipmap (range 0x0 0x10) (repeat 0xF 0))
  :address-register 0x0000
  :pc 0
  :stack '()
  :delay-timer nil
  :sound-timer nil
}))

; Utilities

(defn get-byte [x n]
    (case n
      1 (bit-shift-right x 8)
      2 (bit-and x 0xFF)
      nil))

(defn get-nibble [x n]
  (let [
    bite (get-byte x (if (> n 2) 2 1))
  ]
  (case n
    1 (bit-shift-right bite 4)
    2 (bit-and bite 0xF)
    3 (bit-shift-right bite 4)
    4 (bit-and bite 0xF)
    nil)))

; Memory

(defn fetch-nxt-instruction [state]
  (let [
    instr-ptr (state :pc)
  ]
    (+ 
      (* 0x100 (get-in state [:memory instr-ptr]))
      (get-in state [:memory (inc instr-ptr)]))))

; Registers     

(defn read-register [state register]
  (get-in state [:registers register]))

(defn update-register [state register value-fn]
  (update-in state [:registers register] #(value-fn %)))

; Opcodes

; Writes constant to register
(defn op6 [state register constant]
  (update-register state register (constantly constant)))

; Adds summand to register (no CF)
(defn op7 [state register summand]
  (update-register state register (partial + summand)))

; Writes value of reg2 to reg1
(defn op8-assign [state reg1 reg2]
  (update-register state reg1 (constantly (read-register state reg2))))

(defn op8-and [state reg1 reg2]
  (update-register state reg1 (partial bit-and (read-register state reg2))))

(defn op8-or [state reg1 reg2]
  (update-register state reg1 (partial bit-or (read-register state reg2))))

(defn op8-xor [state reg1 reg2]
  (update-register state reg1 (partial bit-xor (read-register state reg2))))

(defn op8-add [state reg1 reg2]
  (let [
    val1 (read-register state reg1)
    val2 (read-register state reg2)

    ; set carry flag if any corresponding bits are the same and 1
    cf (if (= 0 (bit-and val1 val2)) 0 1)
  ]
  (->
    (update-register state reg1 (partial + (read-register state reg2)))
    (update-register 0xF (constantly cf)))))

(defn op8-subtract [state reg1 reg2]
  (let [
    val1 (read-register state reg1)
    val2 (read-register state reg2)

    ; borrow flag is set when there is NO borrow, and cleared otherwise
    ; borrow is when a bit in val2 is 1 and the corresponding bit in (xor val1 v2) is 1
    bf (if (->> (bit-xor val1 val2) (bit-and val2) (zero?)) 1 0)
  ]
  (->
    (update-register state reg1 #(- %1 (read-register state reg2)))
    (update-register 0xF (constantly bf)))))
  

; Instruction parsing

(defn op6-family [word]
  (let [
    reg (get-nibble word 2)
    constant (get-byte word 2)
  ]
   (list op6 reg constant)))

(defn choose-opcode [word]
  (case 
    (get-nibble word 1)
    0 nil
    1 nil
    2 nil
    3 nil
    4 nil
    5 nil
    6 (op6-family word)
    7 nil
    8 nil
    9 nil
    nil))
    
; I/O execution

(defn load-program! [program] 
  (swap! vm
    update-in [:memory] (-> program (vec) (constantly))))

(defmulti read-program! 
  #(if (= String (class %)) "path" "data"))

  (defmethod read-program! "path" [path]
    (let [
        program (byte-array (int 1e3)) 
      ]
      (->
        (new java.io.FileInputStream path)
        (.read program))
        (load-program!)))

  (defmethod read-program! "data" [data]
    (load-program! data))

; REPL 
(defn execute! [init-state]
  (loop [state init-state]
    (let [
      nxt-state
        (swap! vm merge
          (let [
            nxt-instr (fetch-nxt-instruction state)
            nxt-opcode (choose-opcode nxt-instr)
            n-state (apply (first nxt-opcode) (cons state (rest nxt-opcode)))
          ]
            (do
              (print nxt-instr)
              (print nxt-opcode)
              (print n-state )
              (update-in n-state [:pc] + 2))))
    ]
    (do
      (println "\n")
      ;(print "Nxt state: " nxt-state "\n\n")
      (if (> (nxt-state :pc) (-> (nxt-state :memory) (count) (- 2)))
        nxt-state
        (recur @vm))))))

; Entrypoint
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (read-program! (args 0))
    (execute!)))
