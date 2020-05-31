(ns chip8.disp
  (:gen-class)
  (:use [clojure.java.shell :only [sh]]))

(defn clean-sh []
  (sh "clear"))

(defn pixel-addr [x y]
  (+ x (* y 64)))

(defn sprite-byte [state x y]
  (->> (* y 8) (+ (quot x 8))))

(defn bin-vec [n]
  (loop [bits '() base n]
    (if (zero? base)
      (->>
        (repeat (- 8 (count bits)) 0)
        (concat bits)
        (reverse))
      (recur
        (cons (if (zero? (rem base 2)) 0 1) bits)
        (quot base 2)))))

(defn pixel-value [slice]
  (loop [bit 7 n 0 i 0]
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

(defn number-pixel-row [n]
  (let [
    on-pixel "▪▪"
    off-pixel "  "
  ]
    (loop [row '() pixel n bits-left 8]
      (if (= 0 bits-left)
        (reduce str row)
        (recur 
          (cons (if (= 1 (bit-and pixel 0x1)) on-pixel off-pixel) row)
          (bit-shift-right pixel 1)
          (dec bits-left))))))

(defn render-screen [state]
  (let [
    on-pixel "▪▪"
    off-pixel "  "
    screen (state :display)
    rows (partition 64 screen)
    pixel-value #(if (zero? %) off-pixel on-pixel)
  ]
  (do 
    (->>
      (map 
        #(->> (map pixel-value %) (reduce str))
        rows)
      (reduce #(str %1 "\n" %2))))))
