(ns kigen.pos)

(defn indexed
  "Indexes a collection, i.e. associating ordinal numbers to its elements."
  [coll]
  (into {} (map vector (iterate inc 0) coll))) ;list, vectors index from 0

(defn positions
  "All positions of elements satisfying predicate v in an indexed collection."
  [pred indxd] (for [[i v] indxd :when (pred v)] i))

(defn pos
  [pred indx] (first (positions pred indx)))
