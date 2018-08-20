(ns advent-of-code-2015.day12
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]
            [advent-of-code-2015.utils :as utils]))

(defn has-red? [m]
  (some (fn [[k v]]
          (or (= k "red") (= v "red")))
        m))


(defn sum-numbers [m]
  (let [sum (atom 0)]
    (walk/postwalk (fn [form]
                     (if (number? form)
                       (do
                         (swap! sum #(+ % form))
                         form)
                       form))
                   m)
    @sum))


(comment

  ;; Part 1
  (let [s (utils/input "day12.txt")
        json (json/parse-string s)]
    (sum-numbers json))

  ;; Part 2
  (let [s (utils/input "day12.txt")
        json (json/parse-string s)
        filtered-json (walk/postwalk (fn [form]
                                      (if (and (map? form) (has-red? form))
                                        {}
                                        form))
                                    json)]
    (sum-numbers filtered-json)))

