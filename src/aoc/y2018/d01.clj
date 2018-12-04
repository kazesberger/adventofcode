(ns aoc.y2018.d01
  (:require [clojure.string :as str]))

(def puzzle-input (map read-string (str/split (slurp "resources/puzzle-input/y18d01") #"\s+")))

(reduce + 0 puzzle-input)

(count puzzle-input)



(comment
  (reductions + 0 puzzle-input)

  (defn first-duplicate [seenset x]
    (if (seenset x)
      (reduced x)
      (conj seenset x)))

  (reduce first-duplicate #{} (reductions + 0 (cycle puzzle-input)))

  (nth (cycle puzzle-input) 977)
  (second (cycle puzzle-input)))
  
  ;(take-while #(not= 3 %) (range)))

(comment)
  ;(second
  ;  ((fn part2
  ;     ([] (part2 '(0) (reductions + 0 puzzle-input)))
  ;     ([to from]
  ;      (lazy-seq
  ;        (let   [num (first from)
  ;                iteration (rest (if (< (last to) num) (iterate inc num) (iterate dec num)))]
  ;          ;(range)))))))
  ;          (part2 (concat to (take-while #(not= num %) iteration))
  ;                 (rest from)))))))))


  ;(let   [to '(0)
  ;        from '(5 -5 10 -10)]
  ;
  ;  (loop  [num (first from)
  ;          iteration (rest (if (< (last to) num) (iterate inc (last to)) (iterate dec (last to))))]
  ;    (recur (concat to (take-while #(not= % num) iteration) (list num))
  ;           (rest from))))
  ;
  ;(let   [to '(0)
  ;        from '(5 -5 10 -10)
  ;        num (first from)
  ;        iteration (rest (if (< (last to) num) (iterate inc (last to)) (iterate dec (last to))))]
  ;  (concat to (take-while #(not= num %) iteration) (list num)))
  ;
  ;(let   [to '(0 1 2 3 4 5)
  ;        from (rest '(5 -5 10 -10))
  ;        num (first from)
  ;        iteration (rest (if (< (last to) num) (iterate inc (last to)) (iterate dec (last to))))]
  ;  (concat to (take-while #(not= num %) iteration) (list num)))
  ;
  ;(take 3 (iterate dec 0))
  ;(concat '(0 1 2 3) '(2 1) (list 0)))
  ;
              
     
     
