(ns advent-of-code-2015.day09
  (:require [advent-of-code-2015.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))


(defn line->edge [line]
  (let [re #"^(\w+) to (\w+) = (\d+)$"
        [[_ from to distance]] (re-seq re line)]
    [#{from to} (Integer/parseInt distance)]))


(defn ->graph [lines]
  (let [distances (into {} (map line->edge lines))
        locations (reduce (fn [locations [k _]]
                            (set/union locations k))
                          #{}
                          distances)]
    {:distances distances
     :locations locations}))


(defn distance-amounts [{:keys [distances locations]}]
  (->> (combo/permutations locations)
       (map #(partition 2 1 %))
       (map (fn [route]
              (map (fn [[from to]]
                     (get distances #{from to} ::no-route))
                   route)))
       (remove (fn [distances]
                 (some #(= ::no-route %) distances)))
       (map (partial reduce +))))


(defn reduce-distance [rf lines]
  (->> lines
       ->graph
       distance-amounts
       (reduce rf)))



(comment

  ;; Part 1
  (reduce-distance min (utils/split-lines "day09.txt"))

  ;; Part 2
  (reduce-distance max (utils/split-lines "day09.txt")))