(ns advent-of-code-2015.day03
  (:require [advent-of-code-2015.utils :as utils]
            [clojure.set :as set]))


(def directions-legend
  {\> [0 1]
   \^ [-1 0]
   \< [0 -1]
   \v [1 0]})


(defn travel [start directions]
  (reduce
    (fn [positions direction]
      (let [last-position (last positions)
            next-direction (get directions-legend direction)
            next-position (mapv + last-position next-direction)]
        (conj positions next-position)))
    [start]
    directions))


(defn collate [coll]
  (reduce (fn [[evens odds] [even odd]]
            [(if even (conj evens even) evens)
             (if odd (conj odds odd) odds)])
          [[] []]
          (partition-all 2 coll)))


(comment

  ;; Part 1
  (count (set (travel [0 0] (utils/input "day03.txt"))))

  ;; Part 2
  (let [directions (utils/input "day03.txt")
        [santa robot] (collate directions)]
    (count (set/union (set (travel [0 0] santa))
                      (set (travel [0 0] robot))))))
