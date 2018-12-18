(ns aoc.y2018.d09.kazesberger)

; reduce over turns
(defn turns [n-players highest-marble]
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
  (if (zero? (mod (second turn) 23))
    (let [remove-index (mod (- current 7) (count circle))
          ;foo (println circle remove-index)
          removed-marble (nth circle remove-index)
          ;foo (println removed-marble)
          score-2-add (+ (second turn) removed-marble)]
      {:circle (remove #{removed-marble} circle)
       :score (update score (first turn) + score-2-add)
       :current remove-index})
    (let [[a b] (split-at (get-split-index current (count circle)) circle)
          circle (concat a [(second turn)] b)]
      {:circle circle
       :score score
       :current (count a)})))

(defn part-1 [n-players highest-marble]
  (apply max (vals (:score (reduce rf {:score (zipmap (range 1 (inc n-players))
                                                      (repeat 0))
                                       :circle [0]
                                       :current 0} (turns n-players highest-marble))))))

;(part-1 411 71170)
;(part-1 10 1618)

(comment
  (reduce rf {:score (zipmap (range 1 11) (repeat 0)) :circle [0] :current 0} (turns 10 1618))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def highest-marble 500)

(take-while #(< % highest-marble) (iterate #(+ % 23) 23))

(defn get-insertion-index [[last-insert-index marblenumber]]
  (if (zero? (mod marblenumber 23))
    [666 23]
    (let [index (+ 2 last-insert-index)
          roll-over-index (mod index (inc marblenumber))]
      (if (zero? roll-over-index)
        [(inc marblenumber) (inc marblenumber)]
        [roll-over-index (inc marblenumber)]))))

(get-insertion-index [1 1])
(take 25 (iterate get-insertion-index [1 1]))