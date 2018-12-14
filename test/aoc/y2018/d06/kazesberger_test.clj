(ns aoc.y2018.d06.kazesberger-test
  (:require [clojure.test :refer :all]
            [aoc.y2018.d06.kazesberger :refer :all]))

(deftest distance-test
  (is (= 3 (distance [1 1][0 3]) (distance [1 1][2 3]))))