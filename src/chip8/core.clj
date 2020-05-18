(ns chip8.core
  (:gen-class))

(def stack-frame nil)

(def vm {
  
  memory (byte-array 4096)
  registers (zipmap (range 0x0 0x10) (repeat 15 0))
  address-register 0x0000
  stack '()
  delay-timer nil
  sound-timer nil
})

; Utilities

(defn read-register [register]
  (-> (vm "registers") (register)))

(defn update-register! [register value]
  (let [
      curr-registers (vm "registers")
    ]
    (->> 
      (assoc curr-registers register value)
      (assoc vm "registers")
      (set! vm))))

; Opcodes

(defn op6 [register constant]
  (update-register register constant))

(defn op7 [register summand]
  (as-> 
    (read-register register) value
    (+ value summand)
    (update-register! register value)))

(defn op8-assign [reg1 reg2]
  (--> 
    (read-register reg2) 
    (update-register! reg1)))
    
; Emulator execution

(defn load-program [path] 
  (let [
      program (byte-array (int 1e3)) 
    ]
    (->
      (new java.io.FileInputStream path)
      (.read program))))

(defn execute [program]
  (print (vec program)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->
    (load-program (args 0))
    (execute)))
