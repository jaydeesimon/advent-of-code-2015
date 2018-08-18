(ns advent-of-code-2015.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn input [filename]
  (slurp (io/resource filename)))


(defn split-lines [filename]
  (str/split-lines (slurp (io/resource filename))))