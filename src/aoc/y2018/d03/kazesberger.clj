(ns aoc.y2018.d03.kazesberger
  (:require [clojure.string :as str]))

(def puzzle-input  (map #(zipmap [:id :x :y :width :height] %) (partition 5 (map read-string (rest (str/split (slurp "resources/puzzle-input/y18d03") #"[^0-9]+"))))))

(for [x (range 5 (+ 5 (inc 2)))
      y (range 2 (+ 2 (inc 2)))]
  [x y])

(count (filter #(< 1 %) (vals (frequencies (apply concat (map
                                                           (fn [{:keys [id x y width height]}]

                                                             (for [a (range (inc x) (+ (inc x) width))
                                                                   b (range (inc y) (+ (inc y) height))]
                                                               [a b]))
                                                          puzzle-input))))))

(filter #(= 1 %) (vals (frequencies (apply concat (map
                                                    (fn [{:keys [id x y width height]}]

                                                      (for [a (range (inc x) (+ (inc x) width))
                                                            b (range (inc y) (+ (inc y) height))]
                                                        [a b]))
                                                    puzzle-input)))))

(let [claims (map
               (fn [{:keys [id x y width height]}]

                 {id (set (for [a (range (inc x) (+ (inc x) width))
                                b (range (inc y) (+ (inc y) height))]
                            [a b]))})
               puzzle-input)]
  (group-by second claims))

  ;(for [c1 claims
  ;      c2 claims
  ;      :when (and (not= c1 c2)
  ;                 (nil? (seq (clojure.set/intersection c1 c2))))]
  ;  (map :id c1 c2)))
