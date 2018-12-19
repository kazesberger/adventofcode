(ns aoc.y2018.d09.kazesberger-test
  (:require [clojure.test :refer :all]
            [aoc.y2018.d09.kazesberger :refer :all]))


(deftest some-basics
  (is (=
        (reductions #(get-insert-index %1 %2) 0 (range 1 23))
        (map first (take 23 (iterate get-insertion-index [0 0])))))
  (is (= 8317
         (part-1 10 1618)))
  (is (= (apply + (vals (:score (reduce rf {:score (zipmap (range 1 11) (repeat 0)) :circle [0] :current 0} (turns 10 1618)))))
         (apply + (solve-fast 10 1618)))))