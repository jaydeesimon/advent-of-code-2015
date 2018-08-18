(ns advent-of-code-2015.day02
  (:require [clojure.string :as str]
            [advent-of-code-2015.utils :as utils]))


(defn parse-dims [lines]
  (->> lines
       (map #(str/split % #"x"))
       (map (fn [num-strs]
              (mapv #(Integer/parseInt %) num-strs)))))


(defn surface-area [[length width height]]
  (reduce + [(* 2 length width)
             (* 2 width height)
             (* 2 height length)]))


(defn smallest-side-area [dims]
  (let [[a b] (sort dims)]
    (* a b)))


(defn wrapping-paper-area [dims]
  (+ (surface-area dims)
     (smallest-side-area dims)))


(defn volume [[length width height]]
  (* length width height))


(defn smallest-perimeter [dims]
  (let [[a b] (sort dims)]
    (+ (* 2 a) (* 2 b))))


(defn ribbon-length [dims]
  (+ (volume dims) (smallest-perimeter dims)))


(comment

  ;; Part 1
  (let [dims-list (parse-dims (utils/split-lines "day02.txt"))]
    (reduce + (map wrapping-paper-area dims-list)))

  ;; Part 2
  (let [dims-list (parse-dims (utils/split-lines "day02.txt"))]
    (reduce + (map ribbon-length dims-list))))