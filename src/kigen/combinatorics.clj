(ns kigen.combinatorics
  "Combinatorics stuff that is not readily available in
  the contrib package."
  (:require [clojure.math.combinatorics :refer [subsets partitions]]))

(defn singleton?
  "Returns true if the given collection contains exactly 1 element."
  [coll]
  (= 1 (count coll)))

(defn non-empty-subsets
  "All subsets of T as sets, except the empty set."
  [T]
  (remove #{#{}}
          (map set (subsets (seq T)))))

(defn big-enough-partitions
  "All partitions of T with at least n many elements. The elements of the partitions are sets."
  [T n]
  (map (partial map set)
       (filter #(<= n (count %))
               (partitions (seq T)))))
