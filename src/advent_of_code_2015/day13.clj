(ns advent-of-code-2015.day13
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code-2015.utils :as utils]
            [clojure.set :as set]))


(defn parse-line [line]
  (let [re #"^(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\.$"
        [[_ p1 sign points p2]] (re-seq re line)
        points (Integer/parseInt points)]
    {:pair   [p1 p2]
     :points (if (= sign "lose")
               (* -1 points)
               points)}))


(defn input->pair-points [lines]
  (reduce (fn [pair-points {:keys [pair points]}]
            (update
              pair-points
              (set pair)
              (fn [curr]
                (+ (or curr 0) points))))
          {}
          (map parse-line lines)))


(defn names [pair-points]
  (reduce (fn [names [pair _]]
            (set/union names pair))
          #{}
          pair-points))


(defn find-optimal-seating [pair-points]
  (let [names (names pair-points)]
    (->> (combo/permutations names)
         (map (fn [permutation]
                (partition 2 1 (take (inc (count permutation)) (cycle permutation)))))
         (map (fn [permutation]
                (map (fn [pair]
                       (get pair-points (set pair)))
                     permutation)))
         (map (partial reduce +))
         (reduce max))))


(comment

  ;; Part 1
  (find-optimal-seating (input->pair-points (utils/split-lines "day13.txt")))

  ;; Part 2
  (find-optimal-seating (input->pair-points (utils/split-lines "day13-part2.txt"))))