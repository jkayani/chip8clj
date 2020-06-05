(ns chip8.opcodes
  (:gen-class)
  (:require [chip8.utils :refer :all]
            [chip8.vm :refer :all]))

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

(defn opC [state reg constant]
  (->>
    (-> (rand 256) (int))
    (bit-and constant)
    (constantly)
    (update-register state reg)))

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
  (case 
    (get-nibble word 4)
    0x0 (list clear-screen)
    0xE (list exit-subroutine)))

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

; Opcode selection

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
      0xA (cons set-I (addr-opcode word))
      0xB (cons opB (addr-opcode word))
      0xC (cons opC (reg-constant-opcode word))
      0xD (cons draw-sprite (double-reg-constant-opcode word))
      nil)))
