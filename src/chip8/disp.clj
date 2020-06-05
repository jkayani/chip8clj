(ns chip8.disp
  (:gen-class)
  (:use [clojure.java.shell :only [sh]])
  (:require [chip8.utils :refer :all]
            [lanterna.screen :as screen]))

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

(defn draw-screen [state]
  (let [
    on-pixel "◼◼"
    off-pixel "  "
    display (state :display)
    rows (partition 64 display)
    pixel-value #(if (zero? %) off-pixel on-pixel)
  ]
    (mapv
      (fn [row]
        (->> (map pixel-value row) (reduce str)))
      rows)))

; Lanterna stuff

(defn new-display []
  (->> 
    {:cols 120 :rows 30}
  (screen/get-screen :text)))

(def gui (new-display))

(defn start-display! []
  (screen/start gui))

(defn update-gui! [state]
  (let [
    rows (draw-screen state)
  ]
    (do
      (loop [n 0]
        (screen/put-string gui 0 n (rows n))
        (if (= 31 n) nil (recur (inc n))))
      (screen/redraw gui))
      (screen/move-cursor gui 0 0)))

(defn key-pressed? [k]
  (loop [c (screen/get-key gui)]
    (if (= k c)
      true
      (if (nil? c)
        false
        (recur (screen/get-key gui))))))

(defn get-keypress! []
  (screen/get-key-blocking gui))
  
(defn exit-program? []
  (loop [c (screen/get-key gui)]
    (if (= :escape c)
      (do
        (screen/stop gui)
        (System/exit 0))
      (if (nil? c)
        nil
        (recur (screen/get-key gui))))))
