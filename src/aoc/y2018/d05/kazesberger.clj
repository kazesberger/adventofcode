(ns aoc.y2018.d05.kazesberger
  (:require [clojure.string :as str])
  (:import [java.util Calendar]))

(def puzzle-input (slurp "resources/puzzle-input/y18d05"))

(defn abs [n] (max n (- n)))

(def react?
  (fnil (fn [x y] (= 32 (abs (reduce - (map int [x y]))))) 0))

(react? \a \A)

;(defn chain-react
;  ([[:as processed] y] (if (react? (last processed) y) processed (conj processed y)))
;  ([x y] (if (react? x y) [x])))

(defn chain-react [xs y]
  (if (react? (first xs) y) (rest xs) (cons y xs)))

(defn count-reduced [puzzle-input] (count (reduce chain-react '() puzzle-input)))
(count-reduced puzzle-input)

(def alphabeth (zipmap (map char (range 65 (+ 65 26))) (map char (range 97 (+ 97 26)))))

(for [letter alphabeth]
  [letter (count-reduced (remove (set letter) puzzle-input))])