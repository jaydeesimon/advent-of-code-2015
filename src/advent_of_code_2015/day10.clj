(ns advent-of-code-2015.day10
  (:require [clojure.string :as str]))


(defn expand [s]
  (transduce
    (comp
      (partition-by identity)
      (mapcat (fn [group]
                (concat (str (count group)) [(first group)]))))
    (completing conj str/join)
    s))


(comment

  ;; Part 1
  (count (last (take (inc 40) (iterate expand "1113122113"))))

  ;; Part 2
  (count (last (take (inc 50) (iterate expand "1113122113")))))
