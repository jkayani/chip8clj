(ns chip8.vm
  (:gen-class)
  (:require [chip8.utils :refer :all]
            [chip8.disp :refer :all]))

(defn new-vm []
  {
    :memory (repeat 0x200 0)
    :registers (apply sorted-map (interleave (range 0x10) (repeat 0x10 0)))
    :I-addr 0x0000
    :pc 0x200
    :stack '()
    :delay-timer nil
    :sound-timer nil
    :display (vec (repeat (* 64 32) 0))
  })

(def vm (atom (new-vm)))

; Registers     

(defn read-register [state register]
  (get-in state [:registers register]))

(defn update-register [state register value-fn]
  (update-in state [:registers register] #(value-fn %)))

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

(defn jump-to-addr [state addr]
  (assoc state :pc (- addr 2)))

(defn skip-nxt-instruction [state]
  (jump-to-addr state (-> (state :pc) (+ 4))))

(defn call-subroutine [state addr]
  (->
    (update-in state [:stack] #(cons (partial (+ 2 (state :pc))) %))
    (jump-to-addr addr)))

(defn exit-subroutine [state]
  (let [
    nxt-addr (first (state :stack))
  ]
    (->
      (update-in state [:stack] #(rest %))
      (jump-to-addr nxt-addr))))

(defn set-I [state addr]
  (assoc state :I-addr addr))

(defn draw-sprite [state reg1 reg2 height]
  (let [
    x (read-register state reg1)
    y (read-register state reg2)
    mem-origin (state :I-addr)
    changed-memory-addrs (range mem-origin (+ mem-origin height))
    sprite-data (map #(read-memory state %) changed-memory-addrs)
    changed-pixel-start-addrs (map #(pixel-addr x %) (range y (+ y height)))
    old-pixel-data
      (map 
        #(->>
          (subvec (state :display) % (+ % 8))
          (pixel-value))
        changed-pixel-start-addrs)
    new-pixel-data (map bit-xor sprite-data old-pixel-data) 
    pixels-flipped? 
      (->>
        (map 
          #(-> (bit-xor %1 %2) (bit-and %1))
          old-pixel-data
          new-pixel-data)
        (not-every? zero?))
  ]
    (do
      (->
        (update-display 
          state 
          (zipmap changed-pixel-start-addrs new-pixel-data))
        (update-register 0xF (constantly (if pixels-flipped? 1 0)))))))

(defn clear-screen [state]
  (->>
    ((new-vm) :display)
    (assoc state :display)))
