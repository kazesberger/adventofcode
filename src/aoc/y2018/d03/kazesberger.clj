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


(let [claims (take 10 (map
                        (fn [{:keys [id x y width height]}]

                          {id (set (for [a (range (inc x) (+ (inc x) width))
                                         b (range (inc y) (+ (inc y) height))]
                                     [a b]))})
                        puzzle-input))]
  ; into clean-claims #(remove overlaps) claims
  (reduce))

(comment
  (def claims-processed {:valid-claims #{1 2 3}
                         :processed #{[1 1] [2 2]}})
  (fn [{:keys [valid-claims processed]} [id claimed-fields]])

  (fn [{:keys [field #{ids}]} [id claimed-fields]])

  (def claims {
               1 #{[1 1]
                   [2 2]}
               2 #{[3 3]
                   [4 4]}
               3 #{[2 2]
                   [5 5]}}))

(let [claims (mapcat
               (fn [{:keys [id x y width height]}]

                 (map #(vector id %) (for [a (range (inc x) (+ (inc x) width))
                                           b (range (inc y) (+ (inc y) height))]
                                       [a b])))
               puzzle-input)]
  ;(take 100 claims))
  ;(filter (set (range 2 1000)) (map (comp count val) (group-by second (take 100000 claims))))
  (clojure.set/difference (set (map :id puzzle-input)) (set (mapcat #(map first %) (vals (filter #(< 1 ((comp count val) %)) (group-by second claims)))))))
  ;(count (filter #(< 1 ((comp count val) %)) (group-by second claims))))

  ;(for [c1 claims
  ;      c2 claims
  ;      :when (and (not= c1 c2)
  ;                 (nil? (seq (clojure.set/intersection c1 c2))))]
  ;  (map :id c1 c2)))
