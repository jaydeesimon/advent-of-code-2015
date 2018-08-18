(ns advent-of-code-2015.day01
  (:require [clojure.java.io :as io]
            [advent-of-code-2015.utils :as utils]))


(defn paren-vals [parens]
  (map (fn [paren]
         (if (= paren \() 1 -1))
       parens))


(defn count-parens [parens]
  (reduce + (paren-vals parens)))


(defn find-basement-step [parens]
  (transduce
    identity
    (fn
      ([] [0 0])
      ([[_ pos]] pos)
      ([[cnt pos] pv]
       (let [new-cnt (+ cnt pv)]
         (if (= new-cnt -1)
           (reduced [new-cnt (inc pos)])
           [new-cnt (inc pos)]))))
    (paren-vals parens)))


(comment

  ;; Part 1
  (count-parens (utils/input "day01.txt"))

  ;; Part 2
  (find-basement-step (utils/input "day01.txt")))