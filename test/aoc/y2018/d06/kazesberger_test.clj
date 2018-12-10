(ns aoc.y2018.d06.kazesberger-test
  (:require [clojure.test :refer :all]
            [aoc.y2018.d06.kazesberger :refer :all]))

(deftest distance-test
 ; TODO wrong -> distance from [1 1] to [0 3] should be same as [2 3]
  (is (= 3 (distance [1 1][0 3]) (distance [1 1][2 3]))))