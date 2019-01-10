(ns aoc.y2018.d09.kazesberger)

; reduce over turns
(defn turns [n-players highest-marble]
  (map vector (cycle (range 1 (inc n-players))) (range 1 (inc highest-marble))))


(defn modulo-but-zero-is-divisor [num div]
  (let [result (mod num div)]
    (if (zero? result)
      div
      result)))

(defn get-split-index [current c-size]
  (modulo-but-zero-is-divisor (+ 2 current) c-size))

(defn get-insert-index [current c-size]
  (modulo-but-zero-is-divisor (+ 2 current) c-size))

(defn get-circlesize-by-marblenumber [marblenumber]
  (- marblenumber (* 2 (quot marblenumber 23))))

(defonce removed-indizes (atom []))

(defn get-remove-index [last-insert-index c-size]

  (let [index (modulo-but-zero-is-divisor (- last-insert-index 7) c-size)]
    (swap! removed-indizes conj index)
    index))

(comment
  (map (partial get-circlesize-by-marblenumber) (range 1 25)))

(defn anomaly? [marble2insert]
  (zero? (mod marble2insert 23)))

;(defn get-insertion-index [[last-insert-index last-inserted-marble :as foo]]
;  ;(println "get-insertion-index: " foo)
;  (let [c-size (get-circlesize-by-marblenumber last-inserted-marble)
;        marble2insert (inc last-inserted-marble)]
;    ;(println "get-insertion-index/c-size: " c-size (anomaly? marble2insert))
;    (if (anomaly? marble2insert)
;      [(get-remove-index last-insert-index c-size) marble2insert]
;      [(get-insert-index last-insert-index c-size) marble2insert])))

(defn get-insertion-index [[last-insert-index last-inserted-marble :as foo]]
  ;(println "get-insertion-index: " foo)
  (let [marble2insert (inc last-inserted-marble)
        c-size (get-circlesize-by-marblenumber marble2insert)
        ;foo (if anomaly? (println "anomaly: " marble2insert))
        new-current-index (if (anomaly? marble2insert)
                            (get-remove-index last-insert-index c-size)
                            (get-insert-index last-insert-index c-size))]
    [new-current-index marble2insert]))
    ;(println "get-insertion-index/c-size: " c-size (anomaly? marble2insert))

(comment
  (let [last-insert-index 5
        c-size (get-circlesize-by-marblenumber 7)]
    (vector (get-remove-index last-insert-index c-size)
            (get-insert-index last-insert-index c-size)))
  (get-insertion-index [15 15])
  (get-insertion-index [1 1])
  (get-insert-index 1 (get-circlesize-by-marblenumber 3))
  (get-insertion-index [1 1])
  (take 23 (iterate get-insertion-index [1 1])))

(comment
  ;(map #(vector % (quot (inc %) 23)) (range 1 50))
  (get-insert-index 1 3)
  (let [last-insert-index 34
        marblenumber 37]
    [(get-insert-index last-insert-index marblenumber)
     (get-insertion-index [last-insert-index marblenumber])]))


(defn rf [{:as game
           :keys [score circle current]}
          turn]
  ;(println "rf: " turn (anomaly? (second turn)) circle)
  (if (anomaly? (second turn))
    (let [remove-index (get-remove-index current (count circle))
          ;foo (println circle remove-index)
          removed-marble (nth circle remove-index)
          ;foo (println removed-marble)
          score-2-add (+ (second turn) removed-marble)]
      {:circle (remove #{removed-marble} circle)
       :score (update score (first turn) + score-2-add)
       :current remove-index})
    (let [[a b] (split-at (get-split-index current (count circle)) circle)
    ;(let [[a b] (split-at (first (get-insertion-index [current (second turn)])) circle)
          circle (concat a [(second turn)] b)]
     (if (< 35 (second turn) 40) (println current  (count circle) circle))
     {:circle circle
      :score score
      :current (count a)})))

(comment
  (map anomaly? (map second (take 24 (turns 10 1618)))))

(defn part-1 [n-players highest-marble]
  (apply max (vals (:score (reduce rf {:score (zipmap (range 1 (inc n-players))
                                                      (repeat 0))
                                       :circle [0]
                                       :current 0} (turns n-players highest-marble))))))

;(part-1 411 71170)
;(part-1 10 50)
(part-1 10 1618)

(comment
  (reduce rf {:score (zipmap (range 1 11) (repeat 0)) :circle [0] :current 0} (turns 10 50))
  (let [[a b :as foo] (#(split-at (get-insert-index %1 (count %2)) %2) 1 [0 4 2 1 3])]
    (concat a [5] b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (def highest-marble 500)
  (take-while #(< % highest-marble) (iterate #(+ % 23) 23)))

(comment
  (get-insertion-index [1 1])
  (take 23 (iterate get-insertion-index [1 1])))

(defn get-from-insert-seq [[find-index offset :as a] [insertion-index marble :as b]]
  ;(println a b)
  ;(println (neg? find-index) (= insertion-index (+ offset find-index)) (< insertion-index (+ offset find-index)))
  (cond
    ;(zero? (mod offset 23)) [insertion-index 0]
    (neg? find-index) [insertion-index 0]
    (= find-index (+ offset insertion-index)) (reduced marble)
    (> find-index (+ offset insertion-index)) [find-index (inc offset)]
    :else [find-index offset]))

(comment
  (reduce get-from-insert-seq [-1 0] (reverse (take 23 (iterate get-insertion-index [1 1]))))
  (take 25 (iterate get-insertion-index [1 1])))

(defn solve-fast [n-players highest-marble]
  (let [insertions (iterate get-insertion-index [1 1])
        anomalies (take-while #(<= % highest-marble) (iterate #(+ % 23) 23))
        anomaly-turns (map (juxt identity #(modulo-but-zero-is-divisor % n-players)) anomalies)
        get-score #(+ % (reduce get-from-insert-seq [-1 0] (reverse (take % insertions))))
        scores (map #(update % 0 get-score) anomaly-turns)
        playerscore (group-by second scores)
        sum-up (fn [scores] (apply + (map first scores)))]
    ;scores))

    (map sum-up (map second playerscore))))
    ;playerscore))
    ;(map #(update % 0 get-score) anomaly-turns)))

(comment
  (rsubseq)
  (rseq)
  (subse)
  (repli)
  (transient)
  (reduce get-from-insert-seq [-1 0])
  (map #(vector % (mod % 10)) (range 1 12))
  (apply + (map first [[322 0] [533 0] [963 0] [1283 0] [1601 0] [1919 0] [2253 0]]))
  (update [[1 1] [2 2]] 0 inc)
  (let [n-players 9
        highest-marble 50
        lame-turns (map vector (cycle (range 1 (inc n-players))) (range 1 (inc highest-marble)))
        ;turns (map #(vector % (mod % n-players) (range 1 (inc highest-marble))))
        anomalies (take-while #(< % highest-marble) (iterate #(+ % 23) 23))
        anomaly-turns (map #(vector % (mod % n-players)) anomalies)])
    ;[lame-turns anomaly-turns]))
    ;(= [lame-turns turns])))
    ;[ (map second anomaly-turns) anomalies]))

  (apply + (vals (:score (reduce rf {:score (zipmap (range 1 11) (repeat 0)) :circle [0] :current 0} (turns 10 1618)))))
  (map first (solve-fast 10 1618))
  (apply max (map first (solve-fast 10 1618)))
  ;part-1/2:
  (solve-fast 10 1618)
  (def removed-by-solve-fast @removed-indizes)
  (def removed-by-solve-slow @removed-indizes)
  (take 10 removed-by-solve-fast)
  (take 10 removed-by-solve-slow)
  (= removed-by-solve-fast removed-by-solve-slow)
  (reset! removed-indizes [])

  (vals (:score (reduce rf {:score (zipmap (range 1 11) (repeat 0)) :circle [0] :current 0} (turns 10 100))))
  (solve-fast 10 100))

;insertion-indizes correct:
[37 38]
[]

[32 3]
[63 6]
[80 9]
[107 2]
[165 5]
[196 8]
[227 1]
[217 4]
[244 7]
[329 10]
[360 3]
[321 6]
[354 9]
[462 2]
[493 5]
[524 8]
[464 1]
[491 4]
[626 7]
[657 10]
[571 3]
[601 6]
[755 9]
[790 2]

