;;TODO consider using immutable bitsets from https://github.com/clojure/data.int-map
(ns kigen.multab
  "Functions for dealing with abstract multiplication tables,
  i.e. multiplicative elements are represented by their indices in a
  given sequence, and sets of these elements.
  The tables are vectors of vectors (the rows of the table),
  so multiplication is just look up."
  (:require [clojure.set :refer [difference union subset?]]
            [kigen.orbit :as orbit]
            [kigen.sgp :as sgp]))

(defmacro at
  "Convenient accessing of matrix elements."
  [mt i j]
  `((~mt ~i) ~j))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul.
  Computation is done by rows in parallel."
  [xs mul]
  (let [vxs (vec xs)
        indx (zipmap vxs (range (count vxs)))]
    (vec (pmap
          (fn [x] (->> xs
                       (map #(mul % x)) ;left multiplication by x
                       (map indx) ;elt -> index
                       (vec)))
          xs))))

(defn elts
  "Returns the elements of the given multiplication table.
  The elements are just the set of indices from 0 to n-1."
  [mt]
  (set (range (count mt))))

(defn set-mul
  "Setwise multiplication of subsets of a multab. For A and B it returns AB."
  [mt A B]
  (set (for [i A j B] (at mt i j))))

(defn index-period
  "The index-period pair of integers in a vector in a multiplication table."
  [mt x]
  (sgp/index-period x (fn [x y] (at mt x y))))

(defn newelements
  "For a subsemigroup S and a subset X in mt this returns the elements
  (SX union XS) setminus (S union X)."
  [mt S X]
  (if (subset? X S)
    #{}
    (let [T (union S X)]
      (remove T (union (set-mul mt T X)
                       (set-mul mt X T))))))

(defn closure
  "It calculates the closure of base with elements in the set exts."
  ([mt exts] (closure mt #{} exts))
  ([mt base exts]
   (letfn
       [(finished? [[_ exts]] (empty? exts))
        (extend [[base exts]]
          #{[(set  (union base exts)) (set (newelements mt base exts))]})]
     (first
      (orbit/first-solution-single [base exts] extend (fn [x] true) finished?)))))

(defn in-closure?
  "Returns true if an element x is in the closure of sgp by gens"
  ([mt gens x] (in-closure? mt #{} gens x))
  ([mt sgp gens x]
   (letfn
       [(finished? [[sgp gens]]
          (or (contains? sgp x) (contains? gens x)))
        (extend [[sgp gens]]
          #{[(set  (union sgp gens)) (set (newelements mt sgp gens))]})]
     (some?
      (orbit/first-solution-single [sgp gens] extend (fn [x] true) finished?)))))

(defn min-extensions
  "Returns the minimal extensions (by new element) of closed subarray of
  multiplication table mt."
  [mt closedsub]
  (let [complement (difference (elts mt) closedsub)]
    (set (pmap #(closure mt closedsub #{%}) complement))))

(defn subsgps
  "All subsemigroups of an abstract semigroup given by its multiplication
  table"
  [mt]
  (orbit/full-orbit-single [#{}] (partial min-extensions mt)))
