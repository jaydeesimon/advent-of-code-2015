(ns advent-of-code-2015.day07
  (:require [com.stuartsierra.dependency :as dep]
            [clojure.string :as str]
            [advent-of-code-2015.utils :as utils]))


(defn truncate [x]
  (bit-and x 0xFFFF))


(defn and* [x y]
  (truncate (bit-and x y)))


(defn rshift* [x n]
  (truncate (bit-shift-right x n)))


(defn lshift* [x n]
  (truncate (bit-shift-left x n)))


(defn or* [x y]
  (truncate (bit-or x y)))


(defn not* [x]
  (truncate (bit-not x)))


(defn try-parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ s)))


(defn binary-expr [expr result]
  (let [[[_ arg1 fn arg2]] (re-seq #"^(\w+) (\w+) (\w+)$" expr)]
    {:fn fn
     :args [(try-parse-int arg1) (try-parse-int arg2)]
     :result result}))


(defn binary-expr? [s]
  (let [ops ["AND" "OR" "RSHIFT" "LSHIFT"]]
    (some #(str/index-of s %) ops)))


(defn unary-expr [expr result]
  (let [[[_ fn arg]] (re-seq #"(\w+) (\w+)" expr)]
    {:fn fn
     :args [(try-parse-int arg)]
     :result result}))


(defn parse-line [line]
  (let [[[_ left right]] (re-seq #"(.*) -> (.*)" line)]
    (cond
      (str/starts-with? left "NOT")
      (unary-expr left right)

      (binary-expr? left)
      (binary-expr left right)

      :else
      {:fn "SET"
       :args [(try-parse-int left)]
       :result right})))


(def circuit (map parse-line (utils/split-lines "day07.txt")))


(defn build-deps-graph [circuit]
  (let [deps (mapcat (fn [{:keys [args result]}]
                       (map vector (filter string? args) (cycle [result])))
                     circuit)]
    (reduce (fn [g [dep node]]
              (dep/depend g node dep))
            (dep/graph)
            deps)))


(defn resolve* [scalar-or-signal signals]
  (if (int? scalar-or-signal)
    scalar-or-signal
    (get signals scalar-or-signal ::error)))


(defmulti evaluate* :fn)


(defmethod evaluate* "SET" [{[arg] :args result :result} signals]
  (assoc signals result (resolve* arg signals)))


(defmethod evaluate* "OR" [{[arg1 arg2] :args result :result} signals]
  (assoc signals result (or* (resolve* arg1 signals)
                             (resolve* arg2 signals))))


(defmethod evaluate* "AND" [{[arg1 arg2] :args result :result} signals]
  (assoc signals result (and* (resolve* arg1 signals)
                              (resolve* arg2 signals))))


(defmethod evaluate* "RSHIFT" [{[arg1 arg2] :args result :result} signals]
  (assoc signals result (rshift* (resolve* arg1 signals)
                                 (resolve* arg2 signals))))


(defmethod evaluate* "LSHIFT" [{[arg1 arg2] :args result :result} signals]
  (assoc signals result (lshift* (resolve* arg1 signals)
                                 (resolve* arg2 signals))))


(defmethod evaluate* "NOT" [{[arg] :args result :result} signals]
  (assoc signals result (not* (resolve* arg signals))))


(defn evaluate [circuit]
  (let [grouped-by-result (group-by :result circuit)
        g (build-deps-graph circuit)]
    (reduce (fn [signals result]
              (let [[expr] (get grouped-by-result result)]
                (evaluate* expr signals)))
            {}
            (dep/topo-sort g))))


(comment

  ;; Part 1
  (get (evaluate circuit) "a")

  ;; Part 2
  (let [circuit (map parse-line (utils/split-lines "day07-part2.txt"))]
    (get (evaluate circuit) "a")))