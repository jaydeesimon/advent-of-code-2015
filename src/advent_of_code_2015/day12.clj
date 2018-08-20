(ns advent-of-code-2015.day12
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]
            [advent-of-code-2015.utils :as utils]))


(comment

  ;; Part 1
  (let [s (utils/input "day12.txt")
        json (json/parse-string s)
        sum (atom 0)]
    (walk/postwalk (fn [form]
                     (if (number? form)
                       (do
                         (swap! sum #(+ % form))
                         form)
                       form))
                   json)
    @sum))

