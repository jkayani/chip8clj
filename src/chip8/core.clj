(ns chip8.core
  (:gen-class))

(def vm (atom {
  :memory []
  :registers (apply sorted-map (interleave (range 0x0 0x10) (repeat 0x10 0)))
  :address-register 0x0000
  :pc 0
  :stack '()
  :delay-timer nil
  :sound-timer nil
}))

; Utilities

(defn byte-it [n]
  (bit-and n 0xFF))

(defn word-it [n]
  (bit-and n 0xFFFF))

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
    (do 
    (+ 
      (* 0x100 (read-memory state instr-ptr))
      (read-memory state (inc instr-ptr))))))

(defn skip-nxt-instruction [state]
  (update-in state [:pc] (partial + 2)))

(defn call-subroutine [state addr]
  (->
    (update-in state [:stack] #(cons (partial (+ 2 (state :pc))) %))
    (assoc :pc (- addr 2)))) 

(defn exit-subroutine [state]
  (let [
    nxt-addr (first (state :stack))
  ]
    (->
      (update-in state [:stack] #(rest %))
      (assoc :pc (- nxt-addr 2)))))

(defn jump-to-addr [state addr]
  (assoc state :pc addr))

; Registers     

(defn read-register [state register]
  (get-in state [:registers register]))

(defn update-register [state register value-fn]
  (update-in state [:registers register] #(value-fn %)))

; Opcodes

(defn op3 [state register constant]
  (if (= (read-register state register) constant)
    (skip-nxt-instruction state)
    state))

(defn op4 [state register constant]
  (if (not (= (read-register state register) constant))
    (skip-nxt-instruction state)
    state))

(defn op5 [state reg1 reg2]
  (if (= (read-register state reg1) (read-register state reg2))
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

(defn op9 [state reg1 reg2]
  (if (not (= (read-register state reg1) (read-register state reg2)))
    (skip-nxt-instruction state)
    state))

(defn opB [state addr]
  (->>
    (read-register state 0)
    (+ addr)
    (jump-to-addr state)))

; Instruction parsing

(defn const-opcode [word]
  "For opcodes with no input"
  (list word))

(defn addr-opcode [word]
  "For opcodes with an address input"
  (list (bit-and word 0xFFF)))

(defn reg-constant-opcode [word]
  "For opcodes with 1 register and 1 constant as input"
  (list 
    (get-nibble word 2)
    (get-byte word 2)))

(defn double-reg-opcode [word]
  "For opcodes with 2 registers as input"
  (list 
    (get-nibble word 2)
    (get-nibble word 3)))

(defn double-reg-constant-opcode [word]
  "For opcodes with 2 registers and a constant as input"
  (list 
    (get-nibble word 2)
    (get-nibble word 3)
    (get-nibble word 4)))

(defn op0-family [word]
  (list exit-subroutine))

(defn op8-family [word]
  (let [
    code (get-nibble word 4)
    ops {
      0 op8-assign
      1 op8-or
      2 op8-and
      3 op8-xor
      4 op8-add
      5 op8-subtract
      6 op8-shift-right-std
      7 op8-subtract-flipped
      0xE op8-shift-left-std
    }
    operation (ops code)
  ]
    (if (nil? operation)
      (list nil)
      (cons operation (double-reg-opcode word)))))

(defn choose-opcode [word]
  (do 
  (case 
    (get-nibble word 1)
    0 (op0-family word)
    1 (cons jump-to-addr (addr-opcode word))
    2 (cons call-subroutine (addr-opcode word))
    3 (cons op3 (reg-constant-opcode word))
    4 (cons op4 (reg-constant-opcode word))
    5 (cons op5 (double-reg-opcode word))
    6 (cons op6 (reg-constant-opcode word))
    7 (cons op7 (reg-constant-opcode word))
    8 (op8-family word)
    9 (cons op9 (double-reg-opcode word))
    0xB (cons opB (addr-opcode word))
    nil)))
    
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
            nxt-state 
            (do 
              (println (read-memory state (state :pc)))
              (println nxt-instr) 
              (apply (first nxt-opcode) (cons state (rest nxt-opcode))))
          ]
            (do
              (printf "Instruction: %s\n" (Integer/toHexString nxt-instr))
              (print nxt-opcode)
              (print nxt-state)
              (update-in nxt-state [:pc] + 2))))
    ]
    (do
      (println "\n\n")
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
