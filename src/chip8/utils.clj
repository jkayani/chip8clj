(ns chip8.utils
  (:gen-class))

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