(ns kigen.pos
  "Getting the indices of elements in vectors.")

(defn positions
  "All positions of elements of a vector satisfying a predicate."
  [pred v]
  (for [[index element] (map vector (range) v)
        :when (pred element)]
    index))

(defn pos
  "Getting the index of the first element of a vector satisfying a predicate."
  [pred v]
  (first (positions pred v)))

;TODO SPEC
