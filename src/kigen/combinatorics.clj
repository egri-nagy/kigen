(ns kigen.combinatorics
  "Combinatorics stuff that is not readily available in
  the contrib package."
  (:require [clojure.math.combinatorics :refer [subsets partitions]]))

;; just a 'terminology' function for better readability
(defn singleton?
  "Returns true if the given collection contains exactly 1 element."
  [coll]
  (= 1 (count coll)))

;; simply removing the empty set from the collection of subsets
(defn non-empty-subsets
  "All subsets of T as sets, except the empty set."
  [T]
  (remove #{#{}}
          (map set (subsets (seq T)))))

;; partitions has the :min feature, so no need for filtering a bigger collection
(defn big-enough-partitions
  "All partitions of T with at least n many elements.
   The elements of the partitions are sets."
  [T n]
  (map (partial map set)
       (partitions (seq T) :min n)))