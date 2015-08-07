(ns kigen.pos)

;; Finding positions of elements in a collection.
;; adapted from The Joy of Clojure, Second Edition,
;; Manning Publications 2014, pp111-113

(defn index [coll]
  (cond (set? coll) (map vector coll coll) ;elements in set index themselves
        (map? coll) (seq coll) ;seq-ing a map gives key value pairs in vectors
        :else (map vector (iterate inc 0) coll))) ;list, vectors index from 0

(defn positions
  [pred indx] (for [[i v] indx :when (pred v)] i))

(defn pos
  [pred indx] (first (positions pred indx)))
