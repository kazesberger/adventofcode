(ns aoc.y2018.d09.kazesberger)

(def n-players 10)
(def highest-marble 16)
;(def n-marbles 1619)


(comment
  (range 1 highest-marble)
  (iterate take-turn {:score (zipmap (range 1 (inc n-players)) (repeat 0))
                      :circle [0]
                      :players-turn 1}

    (take 15 (cycle (range 1 (inc n-players))))
    (range 1 (inc highest-marble))))

; reduce over turns
(def turns
  (map vector (cycle (range 1 (inc n-players))) (range 1 (inc highest-marble))))


(defn get-split-index [current c-size]
  (let [index (+ 2 current)
        roll-over-index (mod index c-size)]
    (if (zero? roll-over-index)
      c-size
      roll-over-index)))

(defn rf [{:as game
           :keys [score circle current]}
          turn]
  (let [[a b] (split-at (get-split-index current (count circle)) circle)
        circle (concat a [(second turn)] b)]
    {:circle circle
     :score score
     :current (count a)}))

(comment
  (reduce rf {:score {} :circle [0] :current 0} turns)
  (let [[a b :as foo] (#(split-at (get-split-index %1 (count %2)) %2) 1 [0 4 2 1 3])]
    (concat a [5] b)))


(comment
  (split-at 1 [0])
  (split-at 1 [0 1])
  (#(split-at (mod (+ 2 (count %1)) (count %1))  %1) [0 1])
  (#(split-at (mod (+ 2 (count %1)) (count %1))  %1) [0 2 1])

  (#(split-at (mod (+ 2 %1) (inc (count %2)))  %2) 1 [0 2 1])
  ;(#(split-at (mod (+ 2 %1) (inc (count %2)))  %2) 3 [0 2 1 3])
  (#(split-at (mod (inc %1) (inc (count %2)))  %2) 1 [0 2 1]) ; 0 1 2 -> curren=1 / count=3 -> split-at
  (#(split-at (mod (inc %1) (inc (count %2)))  %2) 3 [0 2 1 3])

  (#(split-at (get-split-index %1 (count %2)) %2) 1 [0 2 1]) ;ok
  (#(split-at (get-split-index %1 (count %2)) %2) 3 [0 2 1 3]) ;ok
  (#(split-at (get-split-index %1 (count %2)) %2) 1 [0 4 2 1 3])) ;ok

