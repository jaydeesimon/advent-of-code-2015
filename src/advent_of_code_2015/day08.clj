(ns advent-of-code-2015.day08
  (:require [clojure.string :as str]
            [advent-of-code-2015.utils :as utils])
  (:import (org.apache.commons.text StringEscapeUtils)))


(defn code-length [s]
  (.codePointCount s 0 (.length s)))


(defn hex->escaped-octal [hex]
  (->> (Long/parseLong hex 16)
       Long/toOctalString
       (str "\\")))


(defn unescape [s]
  (StringEscapeUtils/unescapeJava s))


(defn escape [s]
  (StringEscapeUtils/escapeJava s))


(defn replace-escaped-hex [s]
  (str/replace
    s
    #"\\x([a-f0-9][a-f0-9])"
    (fn [[_ hex]]
      (hex->escaped-octal hex))))


(defn remove-surrounding-quotes [s]
  (-> (str/replace s #"^\"" "")
      (str/replace #"\"$" "")))


(defn memory-length [s]
  (-> (replace-escaped-hex s)
      remove-surrounding-quotes
      unescape
      count))


(defn remaining-char-count [strings]
  (let [code-lengths (reduce + (map code-length strings))
        memory-lengths (reduce + (map memory-length strings))]
    (- code-lengths memory-lengths)))


(defn encode [s]
  (str "\"" (escape s) "\""))


(defn remaining-char-count2 [strings]
  (let [new-code-lengths (reduce + (map (comp code-length encode) strings))
        original-code-lengths (reduce + (map code-length strings))]
    (- new-code-lengths original-code-lengths)))

(comment

  ;; Part 1
  (let [strings (utils/split-lines "day08.txt")]
    (remaining-char-count strings))


  ;; Part 2
  (let [strings (utils/split-lines "day08.txt")]
    (remaining-char-count2 strings)))