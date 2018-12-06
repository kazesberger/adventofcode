(ns aoc.y2018.d06.kazesberger
  (:require [clojure.string :as str])
  (:import [java.util Calendar]))

(def puzzle-input (map (comp vec (partial map read-string)) (map #(str/split % #"\,\s") (str/split-lines (slurp "resources/puzzle-input/y18d06")))))

(map #(apply + %) puzzle-input)

(defn abs [n] (max n (- n)))

(defn abs-diff [x y] (abs (- x y)))

(defn distance [[x y] [a b]]
  (+ (abs-diff x a) (abs-diff y b)))

(map #(distance (first puzzle-input) %) puzzle-input)

(for [loc puzzle-input]
  (map #(distance loc %) puzzle-input))


(comment
  (distance [1 1] [2 4]))
