(ns advent-of-code-2015.day05
  (:require [advent-of-code-2015.utils :as utils]
            [clojure.string :as str]))


(defn consecutive-letters-n? [n s]
  (let [n (inc n)]
    (some (fn [group]
            (= (first group) (last group)))
          (partition n 1 s))))


(defn three-or-more-vowels? [s]
  (let [vowel? #{\a \e \i \o \u}
        num-vowels (->> (frequencies s)
                        (filter (comp vowel? first))
                        (map second)
                        (reduce +))]
    (>= num-vowels 3)))


(defn contains-negatives? [s]
  (let [negatives ["ab" "cd" "pq" "xy"]]
    (boolean (some #(str/index-of s %) negatives))))


(defn nice? [s]
  ((every-pred
     (partial consecutive-letters-n? 1)
     three-or-more-vowels?
     (complement contains-negatives?))
    s))


(defn count-pairs [pair s]
  (let [ps (apply str pair)]
    (loop [cnt 0
           idx 0]
      (let [fnd-idx (str/index-of s ps idx)]
        (if (not fnd-idx)
          cnt
          (recur (inc cnt) (+ fnd-idx 2)))))))


(defn contains-pair-at-least-twice? [s]
  (let [pairs (set (partition 2 1 s))]
    (->> (map #(count-pairs % s) pairs)
         (some #(>= % 2)))))


(defn nice2? [s]
  ((every-pred
     contains-pair-at-least-twice?
     (partial consecutive-letters-n? 2))
    s))


(comment

  ;; Part 1
  (let [words (utils/split-lines "day05.txt")]
    (count (filter nice? words)))

  ;; Part
  (let [words (utils/split-lines "day05.txt")]
    (count (filter nice2? words))))
