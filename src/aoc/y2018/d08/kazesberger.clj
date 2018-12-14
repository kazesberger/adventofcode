(ns aoc.y2018.d08.kazesberger
  (:require [clojure.string :as str]))

(def pinput (mapv read-string (str/split (slurp (str "resources/puzzle-input/y18d08")) #"\s+")))



;(take-child-nodes [coll])
  ;(take (+ 2 (first coll) (second coll))))

;(defn get-children [node]
;  (let [child-count (first node)
;        metadata-count (second node)]
;    (->> node
;         (drop 2)
;         (drop-last metadata-count)
;         (take-child-nodes child-count))))

(defn get-metadata-of-childless-node [node]
  (take (second node) (drop 2 node)))

;(defn flatten-nodes [node agg]
;  (if (not (zero? (first node)))
;    (flatten-nodes)
;    (apply + agg (get-metadata node))))

; consume tree bottom-up into [[id pid metadata][id pid metadata]..]

(defonce next-id (atom 1))
(comment
  (swap! next-id inc)
  (reset! next-id 1)
  @next-id)

(defrecord Node [id pid metadata])

;(defn consume-tree [node childless-nodes pid]
;  (if (zero? (first node))
;    (let [childless-node (->Node (swap! next-id inc) pid (take (second node) (drop 2 node)))]
;      (consume-tree (drop (+ 2 (second node)) node) (conj childless-nodes childless-node) pid))
;    (consume-tree)))

    ;(consume-tree (drop 2 node) childless-nodes (swap! next-id inc))))


(defn take-childless-node
  "produces [childless-child-node [rest-of-parent]]"
  [{:keys [node id parsed-nodes] :as layer}]
  (println "take-childless")
  (let [childless-child-node (take (+ 2 (nth node 3)) (drop 2 node))]
    (-> layer
        (update :parsed-nodes #(conj % (->Node (swap! next-id inc) id (get-metadata-of-childless-node childless-child-node))))
        (update :node #(concat [(dec (first %1)) (second %1)] (drop (+ 2 (count childless-child-node)) %1))))))
    ;{:parsed-nodes (conj parsed-nodes (->Node (swap! next-id inc) id (get-metadata childless-child-node)))
    ; :node         (concat [(dec (first node)) (second node)] (drop (+ 2 (count childless-child-node)) node))
    ; :id           id)))

(comment
  (take-childless-node {:parsed-nodes [] :node [1 1 0 2 10 10 20] :id 0}))


(defn drill-down [{:keys [node id parsed-nodes pid] :as layer}]
  ;(println (dissoc layer :node))
  ;(println layer)
  (loop [node node
         pid pid
         parsed-nodes parsed-nodes]
    ;(println "drilling down: " (first node) "children left.")
    ;(println "pn: " parsed-nodes)
    (if (zero? (first node))
      (let [leaf-node-numbers (take (+ 2 (second node)) node)
            rest (drop (count leaf-node-numbers) node)]
        ;(println "got no children left - i'm leaf: " leaf-node-numbers)
        ;(println "got no children left - i'm rest: " rest)
        {:node rest
         :parsed-nodes (conj parsed-nodes (->Node id pid (get-metadata-of-childless-node leaf-node-numbers)))
         :id pid})
      (let [rest-layer-without-1st-child
            (drill-down {:node         (drop 2 node)
                         :id           (swap! next-id inc)
                         :pid          id
                         :parsed-nodes parsed-nodes})]

          (recur (concat [(dec (first node)) (second node)] (:node rest-layer-without-1st-child))
                 pid
                 (:parsed-nodes rest-layer-without-1st-child))))))

(defn part-1 []
  (let [pns (:parsed-nodes (drill-down {:parsed-nodes [] :node pinput :id (reset! next-id 1) :pid -1}))]
    (apply + (mapcat :metadata pns))))

(part-1)


(comment
  (get-metadata-of-childless-node '(0 1 40))
  (drill-down {:parsed-nodes [] :node [2 1 0 1 99 2 2 0 1 30 0 1 40 20 20 10] :id (reset! next-id 1) :pid -1})
  (drill-down {:parsed-nodes [] :node pinput :id (reset! next-id 1) :pid -1}))

(comment
      {:node         (concat [(dec (first node)) (second node)] (:node rest-layer-without-1st-child))
       :parsed-nodes (:parsed-nodes rest-layer-without-1st-child)
       :id           id
       :pid          pid})

;(defn drill-down
;  [{:keys [node id pid] :as node-layer} [:as parsed-nodes]]
;  (if (zero? (first node))
;    (conj parsed-nodes (->Node id pid (get-metadata node)))
;    (if (zero? (nth node 2))
;      (take-childless-node node-layer)
;      (drill-down {:node (drop 2 node) :id (swap! next-id inc) :pid id}))))




(comment
  (concat 1 2 [3 4])
  (let [node [1 1 0 2 10 10 20]
        id 0]
    (take (+ 2 (nth node 3)) (drop 2 node)))
  (take-childless-node {:parsed-nodes [] :node [1 1 0 2 10 10 20] :id 0})
  (->Node 1 2 [3 4 5]))


(nth [1 2 3 4 5] 3)

(defn get-body-size [node-body]
  (+ (first node-body) (second node-body) (get-body-size (drop 2 node-body))))

;(defn metadata-sum [node]
;  (let [children (get-children node)]
;    (if (seq children)
;      (map metadata-sum (chil)))))

(comment
  (get-children [0 2 1 2 3 4 5])
  (get-metadata-of-childless-node [0 2 1 2 3 4 5])
  (get-children [2 2 1 2 3 4 5])
  (get-metadata-of-childless-node [2 2 1 2 3 4 5]))
