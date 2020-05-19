(ns chip8.core
  (:gen-class))

(def stack-frame nil)

(def vm (atom {
  "memory" (byte-array 4096)
  "registers" (zipmap (range 0x0 0x10) (repeat 15 0))
  "address-register" 0x0000
  "stack" '()
  "delay-timer" nil
  "sound-timer" nil
}))

; Utilities

(defn read-register [register]
  (get-in @vm ["registers" register]))

(defn update-register! [register value-fn & args]
  (swap! vm
    (fn [current-vm] 
      (update-in current-vm ["registers" register] #(value-fn %)))))

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
 
    
; Emulator execution

(defn load-program [path] 
  (let [
      program (byte-array (int 1e3)) 
    ]
    (do
      (->
        (new java.io.FileInputStream path)
        (.read program))
      (swap! vm
        #(update-in @vm ["memory"] (constantly program))))))

(defn execute []
  (vec (@vm "memory")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
    (load-program (args 0))
    (execute)))
