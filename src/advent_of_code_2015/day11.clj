(ns advent-of-code-2015.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def imaginary-digits "0123456789abcdefghijklmnop")
(def normalized-digits "abcdefghijklmnopqrstuvwxyz")


(def imaginary->normalized (zipmap imaginary-digits normalized-digits))
(def normalized->imaginary (zipmap normalized-digits imaginary-digits))


(defn password-inc [password]
  (let [radix 26
        normalized (map normalized->imaginary password)
        n (Long/parseLong (str/join normalized) radix)
        password' (map imaginary->normalized (Long/toString (inc n) radix))]
    (str/join password')))


(defn rises-three? [password]
  (let [ascii (map int password)]
    (->> (partition 3 1 ascii)
         (some (fn [[lo med hi]]
                 (and (= (dec hi) med)
                      (= (dec med) lo))))
         boolean)))


(defn contains-negatives? [password]
  (let [negatives #{\i \o \l}]
    (boolean (seq (set/intersection negatives (set password))))))


(defn contains-at-least-two-diff-pairs? [password]
  (let [pairs (->> (partition 2 1 password)
                   frequencies
                   (filter (fn [[[a b] v]]
                             (= a b)))
                   count)]
    (>= pairs 2)))


(defn valid? [password]
  ((every-pred rises-three?
               (complement contains-negatives?)
               contains-at-least-two-diff-pairs?)
    password))


(defn next-password [password]
  (loop [password (password-inc password)]
    (if (valid? password)
      password
      (recur (password-inc password)))))


(comment

  ;; Part 1
  (next-password "hxbxwxba")

  ;; Part 2
  (next-password (next-password "hxbxwxba")))