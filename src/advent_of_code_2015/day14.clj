(ns advent-of-code-2015.day14
  (:require [advent-of-code-2015.utils :as utils]))


(defn try-parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ s)))


(defn parse-line [line]
  (let [re #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$"
        [[_ & rest]] (re-seq re line)
        [reindeer velocity fly-seconds rest-seconds] (map try-parse-int rest)]
    {:reindeer reindeer
     :velocity velocity
     :fly fly-seconds
     :rest rest-seconds}))


(defn total-fly-seconds [fly-seconds rest-seconds total-seconds]
  (->> (mapcat (fn [action]
                 (if (= action :fly)
                   (take fly-seconds (cycle [:fly]))
                   (take rest-seconds (cycle [:rest]))))
               (cycle [:fly :rest]))
       (take total-seconds)
       (filter #(= % :fly))
       count))


(def reindeers (map parse-line (utils/split-lines "day14.txt")))

(comment

  ;; Part 1
  (let [total-seconds 2503]
    (apply max (map (fn [{:keys [velocity fly rest] :as reindeer}]
                      (* velocity (total-fly-seconds fly rest total-seconds)))
                    reindeers)))

  ;; Part 2 (ugh, this is ugly but don't feel like cleaning it up)
  ;; The general idea is to calculate the distance after every second,
  ;; see who the winners are (there could be more than one because of ties)
  ;; and then award points appropriately.
  (let [total-seconds 2503]
    (reduce (fn [points second]
              (let [reindeer-distances (map (fn [{:keys [reindeer velocity fly rest]}]
                                              {:reindeer reindeer
                                               :distance (* velocity (total-fly-seconds fly rest second))})
                                            reindeers)
                    {:keys [distance]} (apply max-key :distance reindeer-distances)
                    reindeer-winners (filter #(= (:distance %) distance) reindeer-distances)]
                (reduce (fn [points' {:keys [reindeer]}]
                          (update points' reindeer (fnil inc 0)))
                        points
                        reindeer-winners)))
            {}
            (range 1 (inc total-seconds)))))
