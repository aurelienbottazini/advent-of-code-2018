(ns advent-of-code.core
  (:gen-class)
  (:use clojure.data)
  (:require [clojure.java.io :as io
             ]))

;; day 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def day1-frequencies-instructions
  (into []
        (map read-string
             (clojure.string/split-lines
               (slurp (io/resource "day1.txt"))))))

(defn day1-find-repeating-frequency [frequencies-list instructions]
  (let [new-frequency (+ (first frequencies-list) (first instructions))]
      (cond
        (some #(if (= new-frequency %) %) frequencies-list) new-frequency
        :else (recur
                (conj frequencies-list new-frequency)
                (conj (into [] (rest instructions)) (first instructions))))))

;; day 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def day2-ids
  (clojure.string/split-lines
    (slurp (io/resource "day2.txt"))))

(defn map-from-id [amap id]
  (cond
    (empty? id) amap
    (contains? amap (first id)) (recur (update-in amap [(first id)] inc) (rest id))
    :else (recur (assoc amap (first id) 1) (rest id))))

(defn score-for-map [amap]
  (let [score { :2 0 :3 0 }
        values (vals amap)]
    (if (some #{2} values)
      (assoc (if (some #{3} values)
               (assoc score :3 1)
               score) :2 1)
      (assoc (if (some #{3} values)
               (assoc score :3 1)
               score) :2 0))))


(defn day2-scores [ids]
  (map score-for-map
          (map #(map-from-id {} %) ids)))

(defn day2-score []
  (let [ reduced-scores (reduce #(hash-map :2 (+ (:2 %1) (:2 %2)) :3 (+ (:3 %1) (:3 %2)))
                               {:2 0 :3 0}
                               (day2-scores day2-ids)) ]
    (* (:2 reduced-scores) (:3 reduced-scores))))

(defn day2-compare-boxes [abox bbox diff]
  (cond
    (> diff 1) diff
    (empty? abox) diff
    (not= (first abox) (first bbox)) (recur (rest abox) (rest bbox) (inc diff))
    :else (recur (rest abox) (rest bbox) diff))
  )

(defn day2-common-string []
  (let [similar-strings (filter identity
                                (for [v day2-ids]
                                  (first (clojure.set/select
                                           #(= 1 (day2-compare-boxes v % 0))
                                           (into #{} day2-ids)))))]
    (apply str (filter identity (last (diff (char-array (first similar-strings))
                                            (char-array (second similar-strings))))))))

;; day3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord slice [id left top width height])

(defn parse-slice [aslice]
  (let [matches (re-find #"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" aslice)]
    (->slice (Integer. (nth matches 1))
             (Integer. (nth matches 2))
             (Integer. (nth matches 3))
             (Integer. (nth matches 4))
             (Integer. (nth matches 5)))))

(def day3-slices
  (map parse-slice
       (clojure.string/split-lines
         (slurp (io/resource "day3.txt")))))

;(def day3-slices
;  (map parse-slice ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"]))

(defn area [slice]
  (apply assoc {}
         (interleave
           (for [x (range (:left slice) (+ (:left slice) (:width slice)))
                 y (range (:top slice) (+ (:top slice) (:height slice)))]
             (list x y))
           (repeatedly (constantly 1)))))

(defn areas []
  (map area day3-slices))

(defn non-overlapping-areas []
  (into {}
        (filter #(= 1 (second %))
  (reduce #(merge-with + %1 %2) (areas)))))

(defn day3-find-non-overlapping []
  (let [non-overlapping-areas-cache (non-overlapping-areas)]
    (filter
      #(if (empty?
             (clojure.set/difference
               (into #{}
                     (keys
                       (area %)))
               (into #{}
                     (keys
                       non-overlapping-areas-cache))))
         true)
      day3-slices)))



(defn day3-count-overlaps []
  (count
    (filter #(> % 1)
            (vals (reduce #(merge-with + %1 %2) (areas))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& _args]
  (println
    "day1"
    (reduce + 0 day1-frequencies-instructions)
    ;; commented because it takes some time to compute
    ;(find-repeating-frequency '(0) frequencies-instructions)

    "day2"
    (day2-score)
    (day2-common-string)

    "day3"
    (day3-count-overlaps)
    (day3-find-non-overlapping)))
