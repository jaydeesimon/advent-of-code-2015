(ns advent-of-code-2015.day06
  (:require [advent-of-code-2015.utils :as u]
            [clojure.set :as set]))

(defn coordinates [[x1 y1] [x2 y2]]
  (set (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y])))


(defprotocol ChristmasLights
  (on [bulbs corner1 corner2])

  (off [bulbs corner1 corner2])

  (toggle [bulbs corner1 corner2])

  (result [bulbs]))


(defrecord SimpleBulbs [bulbs]
  ChristmasLights
  (on [this corner1 corner2]
    (->SimpleBulbs (set/union (:bulbs this) (coordinates corner1 corner2))))

  (off [this corner1 corner2]
    (->SimpleBulbs (set/difference (:bulbs this) (coordinates corner1 corner2))))

  (toggle [this corner1 corner2]
    (let [bulbs (:bulbs this)]
      (let [coords (coordinates corner1 corner2)]
        (->SimpleBulbs
          (set/difference
            (set/union bulbs coords)
            (set/intersection bulbs coords))))))

  (result [this]
    (count (:bulbs this))))


(defn new-simple-bulbs []
  (->SimpleBulbs #{}))


(defn adjust-brightness [bulbs corner1 corner2 adjust-fn]
  (reduce (fn [bulbs coordinate]
            (update bulbs
                    coordinate
                    (fn [brightness]
                      (let [new-brightness (adjust-fn (or brightness 0))]
                        (if (neg? new-brightness) 0 new-brightness)))))
          bulbs
          (coordinates corner1 corner2)))


(defrecord BrightnessBulbs [bulbs]
  ChristmasLights
  (on [this corner1 corner2]
    (->BrightnessBulbs (adjust-brightness
                         (:bulbs this)
                         corner1
                         corner2
                         inc)))

  (off [this corner1 corner2]
    (->BrightnessBulbs (adjust-brightness
                         (:bulbs this)
                         corner1
                         corner2
                         dec)))

  (toggle [this corner1 corner2]
    (->BrightnessBulbs (adjust-brightness
                         (:bulbs this)
                         corner1
                         corner2
                         (comp inc inc))))

  (result [this]
    (reduce + (vals (:bulbs this)))))


(defn new-brightness-bulbs []
  (->BrightnessBulbs {}))


(defn parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ s)))


(defn instruction->fn [instruction]
  (let [re #"^(.+) (\d+),(\d+) through (\d+),(\d+)$"
        [_ command x1 y1 x2 y2] (map parse-int (first (re-seq re instruction)))
        command->fn {"turn on" #'on "turn off" #'off "toggle" #'toggle}]
    (fn [bulbs]
      (let [action-fn (command->fn command)]
        (action-fn bulbs [x1 y1] [x2 y2])))))


(defn instructions->fn [instructions]
  (apply comp (reverse (map instruction->fn instructions))))


(comment

  ;; Part 1
  (let [instructions (u/split-lines "day06.txt")
        f (instructions->fn instructions)]
    (result (f (new-simple-bulbs))))

  ;; ;; Part 2
  (let [instructions (u/split-lines "day06.txt")
        f (instructions->fn instructions)]
    (result (f (new-brightness-bulbs)))))