(ns chip8.core
  (:gen-class))

(def stack-frame nil)

(def vm (atom {
  :memory []
  :registers (zipmap (range 0x0 0x10) (repeat 15 0))
  :address-register 0x0000
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

; Registers     

(defn read-register [register]
  (get-in @vm [:registers register]))

(defn update-register! [register value-fn & args]
  (swap! vm
    update-in [:registers register] #(value-fn %)))

; Opcodes

; Writes constant to register
(defn op6 [register constant]
  (update-register! register (constantly constant)))

; Adds summand to register (no CF)
(defn op7 [register summand]
  (update-register! register (partial + summand)))

; Writes value of reg2 to reg1
(defn op8-assign [reg1 reg2]
  (update-register! reg1 (constantly (read-register reg2))))

(defn op8-and [reg1 reg2]
  (update-register! reg1 (partial bit-and (read-register reg2))))

(defn op8-or [reg1 reg2]
  (update-register! reg1 (partial bit-or (read-register reg2))))

(defn op8-xor [reg1 reg2]
  (update-register! reg1 (partial bit-xor (read-register reg2))))

; Instruction parsing

(defn op6-family [word]
  (let [
    reg (get-nibble word 2)
    constant (get-byte word 2)
  ]
   (list op6 reg constant)))

(defn choose-opcode [word]
  (case 
    (bit-shift-right word 12)
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

(defmulti read-program 
  #(if (= String (class %)) "path" "data"))

  (defmethod read-program "path" [path]
    (let [
        program (byte-array (int 1e3)) 
      ]
      (->
        (new java.io.FileInputStream path)
        (.read program))
        (load-program!)))

  (defmethod read-program "data" [data]
    (load-program! data))

(defn execute []
  (vec (@vm "memory")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
    (read-program (args 0))
    (execute)))
