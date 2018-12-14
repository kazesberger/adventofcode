(ns aoc.y2018.d07.kazesberger
  (:require [clojure.string :as str]))


(defn parse-input [filename]
  (let [words (map #(str/split % #"\s+") (str/split-lines (slurp (str "resources/puzzle-input/" filename))))]
    (for [w words]
      (clojure.lang.MapEntry/create
        (nth w 7)
        (second w)))))

(def requirements
  "[a b] a needs b
   step-n+1 -> step-n
   step -> condition"
  (parse-input "y18d07"))


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

(comment
  (part-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reqs = a needs b
; aka    step -> cond

; 1) abstract step-time as req. to subsequent steps (if no subsequent steps add artificial 'done'-step)
; 2) abstract step-time as mapping to worker-progress step->started-ts map of exact size (worker slots)

(defn step-finished? [[_ [_ finished-on-sec] :as foo] secs-passed]
  ;(println foo)
  (<= finished-on-sec secs-passed))

(defn work-done [secs-passed {:as workers}]
  (filter #(step-finished? % secs-passed) workers))

(defn work-left [secs-passed {:as workers}]
  (apply dissoc workers (map first (work-done secs-passed workers))))

(comment
  (dissoc {:w1 ["A" 3]
           :w2 ["B" 5]}
          :w1)
  (apply dissoc {:w1 ["A" 3]
                 :w2 ["B" 5]}
          (map first (work-done 3 {:w1 ["A" 3]
                                   :w2 ["B" 5]})))
  (step-finished? [:w1 ["A" 3]] 30)
  (work-done 3 {:w1 ["A" 3]
                :w2 ["B" 5]})
  (work-left 2 {:w1 ["A" 3]
                :w2 ["B" 5]}))

(defn workerkeys [n]
  (->>(range n)
      (map #(str \w %))
      (map keyword)
      (set)))

(defn free-workers [workers workercount]
  (set (remove (set (keys workers)) (workerkeys workercount))))

(def time-estimates (zipmap (map (comp str char) (range 65 (+ 65 26))) (map #(+ 60 %) (range 1 27))))

(comment
  (time-estimates "A"))

(defn start-work [item secs-passed]
  [item (+ secs-passed (time-estimates item))])

(defn assign-work [workers available-steps workercount secs-passed]
  (let [wkeys (workerkeys workercount)]
    (reduce #(assoc %1 (first (free-workers %1 workercount)) (start-work %2 secs-passed))
            workers
            (take (count (free-workers workers workercount)) available-steps))))

(defn assignments [])


(defn assemble-parallel [secs-passed
                         {:as work :keys [done
                                          workers
                                          steps-left]}
                         workercount]
  (println secs-passed work)
  (let [done (into done (map #(get-in % [1 0])) (work-done secs-passed  workers))]
    (if (or (every? (comp zero? count) [steps-left workers])
            (> secs-passed 200000))
      (str/join done)
      (let [unmet-requirements (unmet-requirements done requirements)
            steps-left (remove (set done) steps-left)
            workers (work-left secs-passed workers)
            assigned-steps (take (count (free-workers workers workercount)) (sort (remove (set (keys unmet-requirements)) steps-left)))
            updated-work-assignments (assign-work workers assigned-steps workercount secs-passed)
            next-iteration-of-work {:done       done
                                    :workers    updated-work-assignments
                                    :steps-left (clojure.set/difference (set steps-left) (set assigned-steps))}]
        (recur (inc secs-passed) next-iteration-of-work workercount)))))

(comment
  (keys {:w1 [] :w2 []})
  (map #(get {:w1 []} %) (workerkeys 5))
  (get {:w0 nil :w1 []} :w2)
  (filter #(nil? (val %)) {:w0 nil :w1 []})
  (take 5 (range 3))
  (every? (comp zero? count) [[] {:w1 ["B" 2] :w2 ["C" 3]}])
  (assemble-parallel 0 {:done ["A"] :workers {:w1 ["B" 2] :w2 ["C" 3]} :steps-left []})
  (assemble-parallel 0 {:done ["A"] :workers {} :steps-left ["B"]})
  (into [] (map second) {:a [1 2] :b [3 4]})
  (into [] (map #(get-in % [1 1])) {:a [1 2] :b [3 4]}))


;(defn assemble-parallel [done steps-left reqs time-req time]
;  (if (zero? (count steps-left))
;    (str/join done)
;    (let [unmet-requirements (unmet-requirements done reqs)
;          available-steps (->> steps-left
;                               (remove (set (keys unmet-requirements)))
;                               ;(filter (set (range 0 (inc time))) time-req)
;                               (sort))
;          start-work-on (first available-steps)
;          dependent-items-of-step (filter #(#{start-work-on} (key %)) reqs)
;          time-for-step 3
;          time-reqs (into time-req (map #(clojure.lang.MapEntry/create (val %) (+ time time-for-step)) dependent-items-of-step))]
;      (println done steps-left available-steps)
;      (recur (conj done start-work-on) (disj steps-left start-work-on)))))

(comment
  (first requirements)
  (into {} (map #(clojure.lang.MapEntry/create (val %) (+ 10 3)) (filter #(#{"R"} (key %)) requirements))))

(comment
  (assemble-parallel [] parts 0)

  (assemble ["Q"] #{"T" "K" "L" "G" "J" "M" "S" "Y" "Z" "H" "E" "R" "C" "F" "B" "P" "V" "U" "O" "X" "N" "A" "I" "W" "D"})

  (merge-with str {} requirements))

