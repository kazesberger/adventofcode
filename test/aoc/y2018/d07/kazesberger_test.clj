(ns aoc.y2018.d07.kazesberger-test
  (:require [clojure.test :refer :all]
            [aoc.y2018.d07.kazesberger :refer :all]
            [clojure.string :as str]))

(deftest assembly-termination-cond
  (is (= "AB" (assemble ["A" "B"] [])))
  (is (= "AB" (assemble-parallel 0 {:done ["A" "B"] :workers [] :steps-left []} 2))))

(deftest solutions
  (is (= "ACBDESULXKYZIMNTFGWJVPOHRQ" (part-1))))
  ;(is (= "ACBDESULXKYZIMNTFGWJVPOHRQ" (assemble-parallel 0 {:done [] :workers {} :steps-left parts} 2))))

(deftest step-finished-pred
  (is (not (step-finished? [:w1 ["A" 3]] 0)))
  (is (step-finished? [:w1 ["A" 3]] 3)))

(deftest work-done-test
  (is (not (seq (work-done 0 {:w1 ["A" 3]
                              :w2 ["B" 5]}))))
  (is (= [[:w1 ["A" 3]]] (work-done 3 {:w1 ["A" 3]
                                       :w2 ["B" 5]})))
  (is (= #{[:w1 ["A" 3]]
           [:w2 ["B" 5]]} (into #{} (work-done 5 {:w1 ["A" 3]
                                                  :w2 ["B" 5]})))))
  ;(testing "warn on simultaneously finishing more than one piece as the problem description is not strict enough there"
  ;  ()))

(deftest free-workers-test
  (is (= #{:w0 :w2 :w3 :w4} (free-workers {:w1 ["C" 0]} 5))))

(deftest test-start-work
  (is (= ["A" (+ 61 10)] (start-work "A" 10)))
  (is (= ["Z" (+ 86 10)] (start-work "Z" 10))))

(deftest test-assign-work
  (is (= {:w0 ["A" 1] :w1 ["B" 62]}
         (assign-work {:w0 ["A" 1]} ["B"] 2 0))))

(deftest parallel-assembly
  (testing ""
    (is (= "ABC" (assemble-parallel 0 {:done ["A" "B"] :workers {:w1 ["C" 0]} :steps-left []} 2)))
    (is (= "ABC" (assemble-parallel 0 {:done ["A" "B"] :workers {:w1 ["C" 3]} :steps-left []} 2)))
    (is (= "ABC" (assemble-parallel 3 {:done ["A" "B"] :workers {:w1 ["C" 3]} :steps-left []} 2)))
    (is (= "AC" (assemble-parallel 3 {:done [] :workers {} :steps-left ["A" "C"]} 2)))
    (is (= "ACB" (assemble-parallel 3 {:done [] :workers {} :steps-left ["A" "C" "B"]} 2)))
    (is (= "ACEUBDSLZXYIMKNWTFGJVPOHRQ" (assemble-parallel 0 {:done [] :workers {} :steps-left parts} 5)))))
