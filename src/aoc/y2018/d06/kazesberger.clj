(ns aoc.y2018.d06.kazesberger
  (:require [clojure.string :as str]
            [clojure.set :refer :all])
  (:import [java.util Calendar]))

;(def pinput (map (comp vec (partial map read-string)) (map #(str/split % #"\,\s") (str/split-lines (slurp "resources/puzzle-input/y18d06")))))
(def pinput
  '([1 1]
    [1 6]
    [8 3]
    [3 4]
    [5 5]
    [8 9]))

(defn abs [n] (max n (- n)))
(defn abs-sum [[x y :as coord]]
  (apply + (map abs coord)))
;(map #(distance (first pinput) %) pinput)
;
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

(defn neighbours [[x y :as coord]]
  #{[(inc x) y]
    [(dec x) y]
    [x (inc y)]
    [x (dec y)]})
  ;(disj (into #{} (for [xn (range (dec x) (+ x 2))
  ;                      yn (range (dec y) (+ y 2))]
  ;                  [xn yn])) coord))

; TODO i prolly need the mapping of generation to field-coord
; TODO     which is hard to work with bc intersections?
; TODO     but maybe it is good/needed to be able to render a board (determine field values)
; TODO     but do i need to render a board?) -> i can render board by (- (field [x y] n)
; TODO                                                                   (field [x y] (dec n))
; TODO    intersections are only possible on same generation
; TODO    otherwise there is always a "winner" -> the younger one (min generation)
; TODO    i definitely need to determine (field-size coord generation)

;locs
(defn field [[x y :as coord] generation]
  (if (zero? generation)
      #{coord}
      (conj
        (set (mapcat #(field % (dec generation)) (neighbours coord)))
        coord)))


(defn field-move [[x y :as coord] generation]
  (if (zero? generation)
    #{coord}
    (clojure.set/difference (field coord generation) (field coord (dec generation)))))



(comment
  (field [0 0] 2)
  (field-move [0 0] 2)
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

;(defn board-moves [pinput generation]
;  (let [keyz     (for [x (range (get-in b-dims [0 0]) (inc (get-in b-dims [1 0])))
;                       y (range (get-in b-dims [0 1]) (inc (get-in b-dims [1 1])))]
;                   [x y])
;        board-values]))



(defn render-board [board]
  (let [b-dims (board-dimensions board)]
    (for [x (range (get-in b-dims [0 0]) (inc (get-in b-dims [1 0])))]
       (for [y (range (get-in b-dims [0 1]) (inc (get-in b-dims [1 1])))]
         (println (get board [x y]))))))



(comment
  (defn done? [game]
    (zero? (count (apply clojure.set/intersection board (map :field))))

    (map #(apply + %) pinput)))


    ;(defn abs-diff [x y] (abs (- x y)))
    ;
    ;(defn distance [[x y] [a b]]
    ;  (+ (abs-diff x a) (abs-diff y b)))))

(comment
  (distance [1 1] [2 4]))
