(ns kigen.pos
  "Getting the indices of elements in vectors.")

(defn positions
  "All positions of elements of a vector satisfying a predicate."
  [pred v]
  (for [[index element] (map vector (range) v)
        :when (pred element)]
    index))

(defn position
  "Getting the index of the first element satisfying a predicate."
  [pred v]
  (first (positions pred v)))

(defn index
  "Getting the index of an element in a vector."
  [v elt]
  (first (positions (partial = elt) v)))

;TODO SPEC
