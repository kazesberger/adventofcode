(ns aoc.y2018.d02.kazesberger
  (:require [clojure.string :as str]))

(def puzzle-input (str/split (slurp "resources/puzzle-input/y18d02") #"\s+"))

;(map (partial group-by identity) puzzle-input)
;(vals (map frequencies puzzle-input))

(->> puzzle-input
     (map frequencies)
     (map vals)
     (filter (partial some #{3}))
     (count))
(* 248 25)


(str/join (apply map #(when (= %1 %2) %1) (first (for [box1 puzzle-input
                                                       box2 puzzle-input
                                                       :when (and (not= box1 box2)
                                                                  (= 1 (count (filter true? (map not= box1 box2)))))]
                                                   [box1 box2]))))


(filter #(some (> (count (second %)) 2)%) (group-by identity "xpysotkqubuhefscajodiglvzw"))

