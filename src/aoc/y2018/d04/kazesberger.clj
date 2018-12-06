(ns aoc.y2018.d04.kazesberger
  (:require [clojure.string :as str])
  (:import [java.util Calendar]))

(def puzzle-input  (-> (slurp "resources/puzzle-input/y18d04")
                       (str/split #"\n+")))

(defn parseDate [s]
  (.parse (java.text.SimpleDateFormat. "[yyyy-MM-dd HH:mm]") s))


(def events (->> puzzle-input
                 ;(map #(str/split % #"\s+"))
                 (map #(vector (parseDate (str/join (take 18 %1))) (str/join (drop 19 %1))))
                 (sort-by first)))

(defn sleep? [s]
  (= s "falls asleep"))

(map
  (fn [[_ eventtype :as event]]
    [event (re-seq #"\d+" eventtype)])
  events)

;(def guard-records (partition 2 (partition-by
;                                  (fn [[_ eventtype]]
;                                    (re-seq #"\d+" eventtype))
;                                  events)))
(def guard-records
  (reduce
    (fn [guard-records guard-event-or-sleep-records]
      (let [recordtype (if  (re-seq #"\d+" (second guard-event-or-sleep-records))
                         :guard
                         :sleep)]
        (if (= recordtype :guard)
          (conj guard-records guard-event-or-sleep-records)
          (update guard-records (dec (count guard-records)) conj guard-event-or-sleep-records))))
    []
    events))

(comment
  (update [[1] [2]] (dec 2) conj 3)
  (reduce
    (fn [guard-records guard-event-or-sleep-records]
      (let [recordtype (if  (re-seq #"\d+" (second guard-event-or-sleep-records))
                         :guard
                         :sleep)]
        (if (= recordtype :guard)
            (conj guard-records guard-event-or-sleep-records)
            (do
              (println)
              (update guard-records (dec (count guard-records)) conj guard-event-or-sleep-records)))))
    [] events)
    ;(partition-by
    ;  (fn [[_ eventtype]]
    ;    (re-seq #"\d+" eventtype))
    ;  events))
  (reduce
    (fn [guard-records guard-event-or-sleep-records]
        (let [recordtype (if  (re-seq #"\d+" (second (first guard-event-or-sleep-records)))
                            :guard
                            :sleep)]
             (if (= recordtype :guard)
               (conj guard-records guard-event-or-sleep-records)
               (conj (update guard-records (count guard-records) #(conj % guard-event-or-sleep-records))))))
    []
    '(
      ([#inst"1518-11-19T22:54:00.000-00:00" "Guard #3271 begins shift"])
      ([#inst"1518-11-19T23:04:00.000-00:00" "falls asleep"]
       [#inst"1518-11-19T23:07:00.000-00:00" "wakes up"]
       [#inst"1518-11-19T23:44:00.000-00:00" "falls asleep"]
       [#inst"1518-11-19T23:52:00.000-00:00" "wakes up"])
      ([#inst"1518-11-20T22:56:00.000-00:00" "Guard #433 begins shift"])
      ([#inst"1518-11-20T23:38:00.000-00:00" "falls asleep"] [#inst"1518-11-20T23:40:00.000-00:00" "wakes up"])
      ([#inst"1518-11-21T22:56:00.000-00:00" "Guard #433 begins shift"])
      ([#inst"1518-11-21T23:38:00.000-00:00" "falls asleep"] [#inst"1518-11-21T23:55:00.000-00:00" "wakes up"])
      ([#inst"1518-11-22T23:00:00.000-00:00" "Guard #3271 begins shift"])
      ([#inst"1518-11-22T23:07:00.000-00:00" "falls asleep"] [#inst"1518-11-22T23:57:00.000-00:00" "wakes up"]))))


;([#inst"1518-11-20T22:56:00.000-00:00" "Guard #433 begins shift"])
                 ;([#inst"1518-11-20T23:38:00.000-00:00" "433 falls asleep"] [#inst"1518-11-20T23:40:00.000-00:00" "wakes up"])
                 ;([#inst"1518-11-20T23:38:00.000-00:00" "433 falls asleep"] [#inst"1518-11-20T23:40:00.000-00:00" "wakes up"])
                 ;([#inst"1518-11-20T22:56:00.000-00:00" "Guard #434 begins shift"])
                 ;([#inst"1518-11-20T23:38:00.000-00:00" "434 falls asleep"] [#inst"1518-11-20T23:40:00.000-00:00" "wakes up"])
                 ;([#inst"1518-11-20T22:56:00.000-00:00" "Guard #435 begins shift"])
                 ;([#inst"1518-11-20T23:38:00.000-00:00" "435 falls asleep"] [#inst"1518-11-20T23:40:00.000-00:00" "wakes up"]))))


               ;(partition-by
               ;    (fn [[_ eventtype]]
               ;      (re-seq #"\d+" eventtype))
               ;    events))))

;(take 5 (partition 2 shift-records))

(defn parse-sleep-event [[d eventtype :as sleep-event]]
  (let [cal (java.util.Calendar/getInstance)
        cal (doto cal
              (.setTime d))]
    [(.get cal java.util.Calendar/MINUTE)
     (sleep? eventtype)]))

(defn extract-sleep-minutes [sleep-records]
  (let [parsed-sleep-records (map parse-sleep-event sleep-records)]
    (mapcat (fn [[[sleep-start _] [sleep-end _]]]
              (range sleep-start sleep-end))
            (partition 2 parsed-sleep-records))))

(comment
  (let [parsed-sleep-records '([16 true] [27 false] [40 true] [45 false])]
    (mapcat (fn [[[sleep-start _] [sleep-end _]]]
              (range sleep-start sleep-end))
         (partition 2 parsed-sleep-records)))
  (concat (range 16 27) (range 40 45))
  (let [cal (java.util.Calendar/getInstance)
        d #inst"1518-11-22T23:07:00.000-00:00"
        cal (doto cal
              (.setTime d))]
   (.get cal java.util.Calendar/MINUTE)))

(defn get-timetable [guard-records]
  (map
    (fn [[shiftdate guard-string & sleep-records]]
      (let [shiftday (.format (java.text.SimpleDateFormat. "MM-dd") shiftdate)
            guard-id (first (re-seq #"\d+" guard-string))
            sleep-minutes (extract-sleep-minutes sleep-records)]
        [shiftday guard-id sleep-minutes]))
    guard-records))

;(group-by first (map #(vector (second %1) (nth %1 2)) (get-timetable guard-records)))
(def days-slept (map #(update % 1 (fn [[:as days]]
                                    (count (mapcat last days)))) (group-by second (get-timetable guard-records))))
(def sleepiest-guard (read-string (get (clojure.set/map-invert days-slept) (apply max (map second days-slept)))))

(def sleepiest-minute
  (let [sleep-plan (get (group-by second (get-timetable guard-records)) sleepiest-guard)
        sleepiness-per-minute (frequencies (mapcat last sleep-plan))]

    (get (clojure.set/map-invert sleepiness-per-minute) (apply max (vals sleepiness-per-minute)))))

(* sleepiest-guard sleepiest-minute)

(comment
  (update day 1 (fn [[:as days]] (count (mapcat last days))))

  (def day ["1327"
            [["02-08"
              "1327"
              '(7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50)]
             ["02-16"
              "1327"
              '(13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 57 58)]
             ["03-03"
              "1327"
              '(2
                3
                4
                5
                6
                7
                8
                9
                10
                11
                12
                13
                14
                15
                16
                17
                18
                19
                20
                21
                22
                23
                24
                25
                26
                27
                28
                29
                30
                31
                32
                33
                34
                35
                36
                37
                38
                39
                40
                41
                42
                43)]
             ["04-23" "1327" '(40 41 42 43 44 45 46 47 48 49 50 51 52 57)]
             ["06-28" "1327" '(7 8 9 10)]
             ["07-05" "1327" '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 48 49 50 51 52 53 54)]]]))


;
;[[[date guard-string]] [[ts eventtype]]2+]
;
;[day id #{sleep-minutes}]


(.format (java.text.SimpleDateFormat. "MM-dd") #inst"1518-11-22T23:00:00.000-00:00")


(comment
  (map
    (fn [[_ eventtype]]
       [eventtype (let [guard-id (first (re-seq #"\d+" eventtype))]
                    (if (not (nil? guard-id))
                      guard-id
                      (sleep? eventtype)))])
    events))





(comment
  (map #(first (re-seq #"\d+" (second %)))
       [
        ["foo" "Guard #3271 begins shift"]
        ["foo" "falls asleep"]
        ["foo" "wakes up"]])
  (re-seq #"\d+" "Guard #begins shift")
  (.parse (java.text.SimpleDateFormat. "[yyyy-MM-dd HH:mm]")
          (str/join \space ["[1518-06-06" "23:56]"])))

;(.getTime #inst"1518-10-16T23:48:00.000-00:00")
;[#inst"1518-11-22T23:07:00.000-00:00" " falls asleep"]
;[#inst"1518-11-22T23:57:00.000-00:00" " wakes up"]
(compare     #inst"1518-11-22T23:07:00.000-00:00"
             #inst"1518-11-22T22:57:00.000-00:00")



