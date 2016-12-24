(ns kigen.pos
  "Assigning position numbers to elements in a sequential collection.")

(defn positions
  "All positions of elements satisfying predicate in a vector."
  [pred v]
  (for [[index element] (map vector (range) v)
        :when (pred element)]
    index))

(defn pos
  "Getting the index of the first element satisfying a predicate."
  [pred indxd] (first (positions pred indxd)))
