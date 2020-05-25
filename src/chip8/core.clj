(ns chip8.core
  (:gen-class))

(def vm (atom {
  :memory []
  ;:registers (zipmap (range 0x0 0x10) (repeat 0xF 0))
  :registers (apply sorted-map (interleave (range 0x0 0x10) (repeat 0x10 0)))
  :address-register 0x0000
  :pc 0
  :stack '()
  :delay-timer nil
  :sound-timer nil
}))

; Utilities

(defn byte-it [double-word]
  (bit-and double-word 0xFF))

(defn get-byte [x n]
    (case n
      1 (bit-shift-right x 8)
      2 (byte-it x)
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

(defn read-memory [state addr]
  (-> 
    (get-in state [:memory addr])
    (byte-it)))

(defn fetch-nxt-instruction [state]
  (let [
    instr-ptr (state :pc)
  ]
    (do (print instr-ptr (state :memory instr-ptr))
    (+ 
      (* 0x100 (read-memory state instr-ptr))
      (read-memory state (inc instr-ptr))))))

(defn skip-nxt-instruction [state]
  (update-in state [:pc] (partial + 2)))

; Registers     

(defn read-register [state register]
  (get-in state [:registers register]))

(defn update-register [state register value-fn]
  (update-in state [:registers register] #(value-fn %)))

; Opcodes

(defn op3 [state register constant]
  (if (= register constant)
    (skip-nxt-instruction state)
    state))

(defn op4 [state register constant]
  (if (not (= register constant))
    (skip-nxt-instruction state)
    state))

(defn op5 [state reg1 reg2]
  (if (= reg1 reg2)
    (skip-nxt-instruction state)
    state))

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
    (update-register state reg1 (partial + val2))
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
    (update-register state reg1 #(- %1 val2))
    (update-register 0xF (constantly bf)))))

(defn op8-subtract-flipped [state reg1 reg2]
  (let [
    val1 (read-register state reg1)
    val2 (read-register state reg2)
    bf (if (->> (bit-xor val1 val2) (bit-and val1) (zero?)) 1 0)
  ]
  (->
    (update-register state reg1 (constantly (- val2 val1)))
    (update-register 0xF (constantly bf)))))

(defn op8-shift-right-std [state reg1 reg2]
  (let [
    val1 (read-register state reg1)
    val2 (read-register state reg2)
    lsb (bit-and 0x01 val2)
  ]
  (->
    (update-register state reg1 (constantly (bit-shift-right val2 1)))
    (update-register 0xF (constantly lsb)))))

; Variant of above opcode for SCHIP mode
(defn op8-shift-right-schip [state reg1 reg2]
  (let [
    val1 (read-register state reg1)
    lsb (bit-and 0x01 val1)
  ]
  (->
    (update-register state reg1 (constantly (bit-shift-right val1 1)))
    (update-register 0xF (constantly lsb)))))
  
(defn op8-shift-left-std [state reg1 reg2]
  (let [
    val2 (read-register state reg2)
    msb (-> (bit-and 0x80 val2) (bit-shift-right 7))
    new-val (-> (bit-shift-left val2 1) (byte-it))
  ]
  (->
    (update-register state reg1 (constantly new-val))
    (update-register 0xF (constantly msb)))))

; Variant of above opcode for SCHIP mode
(defn op8-shift-left-schip [state reg1 reg2]
  (let [
    val1 (read-register state reg1)
    msb (-> (bit-and 0x80 val1) (bit-shift-right 7))
    new-val (-> (bit-shift-left val1 1) (byte-it))
  ]
  (->
    (update-register state reg1 (constantly new-val))
    (update-register 0xF (constantly msb)))))

; Instruction parsing

(defn op3-family [word]
  (let [
    reg (get-nibble word 2)
    constant (get-byte word 2)
  ]
    (list op3 reg constant)))

(defn op4-family [word]
  (let [
    reg (get-nibble word 2)
    constant (get-byte word 2)
  ]
    (list op4 reg constant)))

(defn op5-family [word]
  (let [
    reg1 (get-nibble word 2)
    reg2 (get-nibble word 3)
  ]
    (list op5 reg1 reg2)))

(defn op6-family [word]
  (let [
    reg (get-nibble word 2)
    constant (get-byte word 2)
  ]
   (list op6 reg constant)))

(defn op7-family [word]
  (let [
    reg (get-nibble word 2)
    constant (get-byte word 2)
  ]
   (list op7 reg constant)))

(defn op8-family [word]
  (let [
    reg1 (get-nibble word 2)
    reg2 (get-nibble word 3)
    op (get-nibble word 4)
  ]
   (case
    op
    0 (list op8-assign reg1 reg2)
    1 (list op8-or reg1 reg2)
    2 (list op8-and reg1 reg2)
    3 (list op8-xor reg1 reg2)
    4 (list op8-add reg1 reg2)
    5 (list op8-subtract reg1 reg2)
    6 (list op8-shift-right-std reg1 reg2)
    7 (list op8-subtract-flipped reg1 reg2)
    0xE (list op8-shift-left-std reg1 reg2)
    nil)))

(defn choose-opcode [word]
  (case 
    (get-nibble word 1)
    0 nil
    1 nil
    2 nil
    3 (op3-family word)
    4 (op4-family word)
    5 (op5-family word)
    6 (op6-family word)
    7 (op7-family word)
    8 (op8-family word)
    9 nil
    nil))
    
; I/O execution

(defn load-program! [program] 
  (swap! vm
    update-in [:memory] 
      (->> 
        program 
        (vec) 
        (constantly))))

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
      new-state
        (swap! vm merge
          (let [
            nxt-instr (fetch-nxt-instruction state)
            nxt-opcode (choose-opcode nxt-instr)
            nxt-state (apply (first nxt-opcode) (cons state (rest nxt-opcode)))
          ]
            (do
              (print nxt-instr)
              (print nxt-opcode)
              (print nxt-state )
              (update-in nxt-state [:pc] + 2))))
    ]
    (do
      (println "\n")
      ;(print "Nxt state: " nxt-state "\n\n")
      (if (> (new-state :pc) (-> (new-state :memory) (count) (- 2)))
        new-state
        (recur @vm))))))

; Entrypoint
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->>
    (read-program! (args 0))
    (execute!)))
