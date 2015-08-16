(ns kigen.multab
  (:use [clojure.set :only [difference union]]
        [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  [xs mul]
  (let [indices (pos/index (vec xs))]
    (vec (pmap
          (fn [x] (->> xs
                       (map #(mul % x))
                       (map #(pos/pos (partial = %) indices))
                       (vec)))
          xs))))

;; getting the i,j entry of the matrix mt
(defmacro at [mt i j]
  `(nth (nth ~mt ~i) ~j))

;; the resulting set may not contain the generators in general
(defn content
  "Returns the content of sub-array spanned by the elements."
  [mt elts]
  (set (for [i elts j elts] (at mt i j))))

(defn extend-by
  "Extends a closed sub-array by elements exts."
  [mt base exts]
  (let [u (union base exts)]
    (union (set (for [i exts j u] (at mt i j)))
           (set (for [i base j exts] (at mt i j))))))

(defn closure
  "Returns the smallest closed subarray that contains the elements."
  [mt elts]
  (loop [base (union (set elts) (content mt elts))
         exts (difference base elts)]
    (if (empty? exts)
      base
      (let [gens (union base exts)
            nbase (union gens (extend-by mt base exts))
            nexts (difference nbase gens)]
        (recur nbase nexts)))))
