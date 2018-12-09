(ns aoc.y2018.d06.kazesberger
  (:require [clojure.string :as str]
            [clojure.set :refer :all])
  (:import [java.util Calendar]))

;(def pinput (map (comp vec (partial map read-string)) (map #(str/split % #"\,\s") (str/split-lines (slurp "resources/puzzle-input/y18d06")))))
(def pinput '([1 1] [1 6] [8 3] [3 4] [5 5] [8 9]))

(defn abs [n] (max n (- n)))

(defn abs-diff [x y] (abs (- x y)))

;(defn abs-sum [[x y :as coord]]
;  (apply + (map abs coord)))

; TODO wrong -> distance from [1 1] to [0 3] should be same as [2 3]
(defn distance [[x1 y1] [x2 y2]]
  (+ (abs-diff x1 x2) (abs-diff y1 y2)))

(distance [1 1] [2 3])

(defn closest2locs [[:as coord]]
  (let [distances (group-by val (zipmap pinput (map #(distance coord %) pinput)))
        nearest-locs (get distances (apply min (keys distances)))]))

(closest2locs [0 4])

(distance [4 4] [5 5])

;(for [loc pinput]
;  (map #(distance loc %) pinput))

  ;iterate: get-points-with-increasing-range (draw increasingly large "circles")
  ;         subtract intersections
  ;         drop out points that do not increase in field size
  ;
  ;until-ideas:  ("board" full)
  ;              (iteration only))

;game := map of maps: {
;  :locs [ {:as loc :keys [[:as coord] #{:as fieldset}]}* ]
;  :free-board #{ [:as coord]* }
;
;generation := (reduce conj-neighbours #{} coords)




;(def game {:locs       (zipmap pinput (lazy-zipmap (range) neighbours))
;           :free-board (set (for [x (first board-size)
;                                  y (second board-size)]
;                              [x y]))})

; locs could just be a [coord (fn [coord generation])]
; free-board           (fn [locs generation])

(def neighbours
  (memoize
    (fn neighbours [[x y :as coord]]
      #{[(inc x) y]
        [(dec x) y]
        [x (inc y)]
        [x (dec y)]})))
(neighbours [0 0])
  ;(disj (into #{} (for [xn (range (dec x) (+ x 2))
  ;                      yn (range (dec y) (+ y 2))]
  ;                  [xn yn])) coord))

; TODO i prolly need the mapping of generation to field-coord
; TODO     which is hard to work with bc intersections?
; TODO     but maybe it is good/needed to be able to render a board (determine field values)
; TODO     but do i need to render a board?) -> i can render board by (- (field [x y] n)
; TODO                                                                   (field [x y] (dec n))
; TODO    intersections are only possible on same generation
; TODO    otherwise there is always a "winner" -> the older one (min generation)
; TODO    i definitely need to determine (field-size coord generation)

;locs
(def field
   (memoize
     (fn [[x y :as coord] generation]
       (if (zero? generation)
           #{coord}
           (conj
             (set (mapcat #(field % (dec generation)) (neighbours coord)))
             coord)))))


(defn field-move [[x y :as coord] generation]
  (if (zero? generation)
    #{coord}
    (clojure.set/difference (field coord generation) (field coord (dec generation)))))



(comment
  (field [0 0] 2)
  (time (field-move [0 0] 10))
  (mapcat neighbours (neighbours [0 0])))

(defn board-dimensions [pinput]
  (let [
        xmin (apply min(map first pinput))
        xmax (apply max(map first pinput))
        ymin (apply min(map second pinput))
        ymax (apply max(map second pinput))]
    [[xmin ymin] [xmax ymax]]))

(comment
  (board-dimensions pinput)
  (range 1 8)
  (print "a" "b"))

(def loc-relations
  (set (for [loc1 pinput
             loc2 pinput
             :when (not= loc1 loc2)]
         #{loc1 loc2})))

(comment
  (zipmap pinput (map #(field-move % 1) pinput))

  (def tng {[1 1] #{[1 0] [2 1] [1 2] [0 1]},
            [1 6] #{[0 6] [1 5] [1 7] [2 6]},
            [8 3] #{[8 4] [7 3] [9 3] [8 2]},
            [3 4] #{[3 3] [2 4] [4 4] [3 5]},
            [5 5] #{[5 4] [6 5] [5 6] [4 5]},
            [8 9] #{[8 8] [8 10] [9 9] [7 9]}})
  (def foo '(#{[1 0] [2 1] [1 2] [0 1]}
              #{[0 6] [1 5] [1 7] [2 6]}
              #{[8 4] [7 3] [9 3] [8 2]}
              #{[3 3] [2 4] [4 4] [3 5]}
              #{[5 4] [6 5] [5 6] [4 5]}))
  (apply clojure.set/union foo)


  (map (fn [[coord boardvalues]]
         (let [intersection-candidates (map second (dissoc tng coord))]
           [coord (apply clojure.set/union intersection-candidates)])) tng))
           ;[coord intersection-candidates])) tng)


(def ng-fn
  (memoize
    (fn [generation]
      (into {} (map (juxt identity #(field-move % generation)) pinput)))))

(defn get-intersections [gen]
  (apply clojure.set/union
    (for [pair loc-relations]
      (clojure.set/intersection (get (ng-fn gen) (first pair)) (get (ng-fn gen) (second pair))))))

(fn [{:as old-board} gen]
  (into))

(defn gen-board [gen]
  (let [ng (ng-fn gen)
        fields-filled (into {} (mapcat #(zipmap (second %1) (repeat (first %1))) ng))
        fields-with-intersects (into fields-filled (zipmap (get-intersections gen) (repeat "***")))]
    (if (zero? gen)
      fields-with-intersects
      (into fields-with-intersects (gen-board (dec gen))))))

(comment
  (select-keys
    (gen-board 11)
    '([178 287]))
  (count (let [b-dims (board-dimensions pinput)]
           (for [x (range (get-in b-dims [0 0]) (inc (get-in b-dims [1 0])))
                 y (range (get-in b-dims [0 1]) (inc (get-in b-dims [1 1])))]
             [x y])))
  (count
    (select-keys
      (gen-board 30)
      (let [b-dims (board-dimensions pinput)]
        (for [x (range (get-in b-dims [0 0]) (inc (get-in b-dims [1 0])))
              y (range (get-in b-dims [0 1]) (inc (get-in b-dims [1 1])))]
          [x y])))))

(defn transpose [m]
  (apply mapv vector m))

(defn render-board [board]
  (transpose
    (let [b-dims (board-dimensions pinput)]
      (for [x (range (get-in b-dims [0 0]) (inc (get-in b-dims [1 0])))]
        (for [y (range (get-in b-dims [0 1]) (inc (get-in b-dims [1 1])))]
          ;[x y]))))
          (if (get board [x y]) (get board [x y]) "___"))))))

(comment
  (render-board (gen-board 5)
    (gen-board 2)
    (into {:a 1 :b 2 :c 4} {:a 3 :b 3})
    (get-intersections 4)
    (map (get (ng-fn 2) [1 1]))
    (get-intersections 0)
    loc-relations
    (ng-fn 2)))

(defn moves [pinput generation]
  (let [tng (zipmap pinput (map #(field-move % generation) pinput))]))

    ;(for [move tng]
    ;  (clojure.set/difference (second move) ())))


  ; any intersections between any 2 move-sets of same generation are "." coords
  ; we update the inner (older / lower generation value) recursion over the outer layers





  ;(let [keyz     (for [x (range (get-in b-dims [0 0]) (inc (get-in b-dims [1 0])))
  ;                     y (range (get-in b-dims [0 1]) (inc (get-in b-dims [1 1])))]
  ;                 [x y])
  ;      board-values]))






(comment
  (defn done? [game]
    (zero? (count (apply clojure.set/intersection board (map :field))))

    (map #(apply + %) pinput)))
