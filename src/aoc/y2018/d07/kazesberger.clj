(ns aoc.y2018.d07.kazesberger
  (:require [clojure.string :as str]))


(def requirements
  "[a b] a needs b
   step-n+1 -> step-n
   step -> condition"
  (let [words (map #(str/split % #"\s+") (str/split-lines (slurp "resources/puzzle-input/y18d07")))]
    (for [w words]
      (clojure.lang.MapEntry/create
        (nth w 7)
        (second w)))))


(def parts (set (flatten requirements)))

(defn unmet-requirements [done requirements]
  (remove #((set done) (val %)) requirements))

(comment
  (count requirements)
  (count (unmet-requirements ["A" "C" "E" "U"] requirements))
  (frequencies (keys requirements)))

(comment
  (remove #(#{:a :b } (key %)) {:a 1 :b 2 :c 3}))
(defn assemble [done parts]

  (if (zero? (count parts))
    (str/join done)
    (let [unmet-requirements (unmet-requirements done requirements)
          steps-left (remove (set done) parts)
          available-steps (sort (remove (set (keys unmet-requirements)) steps-left))]
      (println done parts available-steps)
      (when (seq available-steps) ; terminal-cond on being stuck
        (recur (conj done (first available-steps)) (disj parts (first available-steps)))))))
      ;[(conj done (first available-steps)) (disj parts (first available-steps))])))

(defn part-1 []
  (assemble [] parts))

(defn assemble-parallel [done steps-left reqs time-req time]
  (if (zero? (count steps-left))
    (str/join done)
    (let [unmet-requirements (unmet-requirements done reqs)
          available-steps (->> steps-left
                               (remove (set (keys unmet-requirements)))
                               ;(filter (set (range 0 (inc time))) time-req)
                               (sort))
          start-work-on (first available-steps)
          dependent-items-of-step (filter #(#{start-work-on} (key %)) reqs)
          time-for-step 3
          time-reqs (into time-req (map #(clojure.lang.MapEntry/create (val %) (+ time time-for-step)) dependent-items-of-step))]
      (println done steps-left available-steps)
      (recur (conj done start-work-on) (disj steps-left start-work-on)))))

(comment
  (first requirements)
  (into {} (map #(clojure.lang.MapEntry/create (val %) (+ 10 3)) (filter #(#{"R"} (key %)) requirements))))

(comment
  (assemble-parallel [] parts 0)

  (assemble ["Q"] #{"T" "K" "L" "G" "J" "M" "S" "Y" "Z" "H" "E" "R" "C" "F" "B" "P" "V" "U" "O" "X" "N" "A" "I" "W" "D"})

  (merge-with str {} requirements))

