(ns advent-of-code-2015.day01
  (:require [clojure.java.io :as io]))

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

  (let [part01 (slurp (io/resource "day01.txt"))]
    (count-parens part01))

  (let [part02 (slurp (io/resource "day01.txt"))]
    (find-basement-step part02)))