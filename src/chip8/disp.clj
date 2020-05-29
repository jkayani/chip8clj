(ns chip8.disp
  (:gen-class)
  (:use [clojure.java.shell :only [sh]]))

(defn thing [arg] arg)

(defn clear-screen []
  (sh "clear"))

(defn beep []
  (sh "echo -ne '\007'"))

(defn draw-sprite [state reg1 reg2 height]
  state)

(defn number-pixel-row [n]
  (let [
    on-pixel ".."
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
    screen (state :display)
    rows (partition 8 screen)
  ]
  (do 
    (->>
      (map 
        #(->> (map number-pixel-row %) (reduce str))
        rows)
      (reduce #(str %1 "\n" %2))))))