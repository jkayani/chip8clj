(ns chip8.disp
  (:gen-class)
  (:use [clojure.java.shell :only [sh]])
  (:require [chip8.utils :refer :all]))

(defn clean-sh []
  (sh "clear"))

(defn pixel-addr [x y]
  (+ x (* y 64)))

(defn bin-vec [n]
  (loop [
    v '() 
    base (byte-it n) 
    bits-left 8
  ]
    (if (= 0 bits-left)
      v
      (recur 
        (cons (bit-and base 0x1) v)
        (byte-it (bit-shift-right base 1))
        (dec bits-left)))))

(defn pixel-value [slice]
  (loop [
    bit 7 
    n 0 
    i 0
  ]
    (if (> i 7)
      n
      (recur 
        (dec bit) 
        (+ n 
          (* (slice i) 
             (-> (Math/pow 2 bit) (int))))
        (inc i)))))

(defn update-display [state pixel-map]
  (let [
    old-display (state :display)
    changed-addrs 
      (->
        (map #(range % (+ % 8)) (keys pixel-map))
        (flatten))
    changes
      (zipmap changed-addrs
        (-> 
          (map bin-vec (vals pixel-map))
          (flatten)))
  ]
    (assoc state :display 
      (reduce-kv assoc old-display changes))))

(defn render-screen [state]
  (let [
    on-pixel "◼◼"
    off-pixel "  "
    screen (state :display)
    rows (partition 64 screen)
    pixel-value #(if (zero? %) off-pixel on-pixel)
  ]
    (do 
      (->>
        (map
          (fn [row]
            (do
              (->> (map pixel-value row) (reduce str))))
            rows)
          (reduce #(str %1 "\n" %2))))))
