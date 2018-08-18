(ns advent-of-code-2015.day04
  (:import (java.security MessageDigest)))


(defn md5-hash [s]
  (let [bytes (.getBytes s "UTF-8")
        md (MessageDigest/getInstance "MD5")]
    (.digest md bytes)))


(defn starts-with-five-zeroes? [ba]
  (and (zero? (aget ba 0))
       (zero? (aget ba 1))
       (zero? (bit-shift-right (aget ba 2) 4))))


(defn starts-with-six-zeroes? [ba]
  (and (zero? (aget ba 0))
       (zero? (aget ba 1))
       (zero? (aget ba 2))))


(defn find-hash [secret test-fn]
  (loop [n 0]
    (let [test (str secret n)]
      (if (test-fn (md5-hash test))
        n
        (recur (inc n))))))

(comment

  ;; Part 1
  (find-hash "yzbqklnj" starts-with-five-zeroes?)

  ;; Part 2
  (find-hash "yzbqklnj" starts-with-six-zeroes?))
