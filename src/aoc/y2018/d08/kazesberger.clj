(ns aoc.y2018.d08.kazesberger
  (:require [clojure.string :as str]))

(def pinput (mapv read-string (str/split (slurp (str "resources/puzzle-input/y18d08")) #"\s+")))

(defn get-metadata-of-childless-node [node]
  (take (second node) (drop 2 node)))

(defonce next-id (atom 1))
(comment
  (swap! next-id inc)
  (reset! next-id 1)
  @next-id)

(defrecord Node [id pid metadata])

(defn parse-flat [{:keys [node id parsed-nodes pid] :as layer}]
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
            (parse-flat {:node         (drop 2 node)
                         :id           (swap! next-id inc)
                         :pid          id
                         :parsed-nodes parsed-nodes})]

          (recur (concat [(dec (first node)) (second node)] (:node rest-layer-without-1st-child))
                 pid
                 (:parsed-nodes rest-layer-without-1st-child))))))

(defn part-1 []
  (let [pns (:parsed-nodes (parse-flat {:parsed-nodes [] :node pinput :id (reset! next-id 1) :pid -1}))]
    (apply + (mapcat :metadata pns))))

(part-1)

(comment
  (get-metadata-of-childless-node '(0 1 40))
  (parse-flat {:parsed-nodes [] :node [2 1 0 1 99 2 2 0 1 30 0 1 40 20 20 10] :id (reset! next-id 1) :pid -1})
  (parse-flat {:parsed-nodes [] :node pinput :id (reset! next-id 1) :pid -1}))


(defn parse-tree [[n-children n-meta & numberz]]
  (loop [n n-children
         numberz numberz
         children []]
    (if (zero? n)
      (let [meta-self (take n-meta numberz)]
        [{:meta meta-self
          :children children}
         (drop n-meta numberz)])
      (let [[slurped-child rest-numberz] (parse-tree numberz)]
        (recur (dec n) rest-numberz (conj children slurped-child))))))

(comment
  (parse-tree [2 1 0 1 99 2 2 0 1 30 0 1 40 1 20 10]))

(defn node-val [{:keys [meta children] :as node}]
  (println node)
  (println (not node) (empty? children))
  (cond
    (not node) 0
    (empty? children) (apply + meta)
    :else (reduce + 0 (map node-val (map #(get children (dec %)) meta)))))

(defn part-2 []
  (node-val (first (parse-tree pinput))))

(part-2)

(comment
  (node-val (first (parse-tree [2 3 0 1 99 2 2 0 1 30 0 1 40 1 2 2 2])))

  (vals (select-keys [1 2 3] (map dec '(1 3)))))