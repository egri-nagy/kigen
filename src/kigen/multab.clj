(ns kigen.multab
  "Functions for dealing with abstract multiplication tables,
  i.e. multiplicative elements are represented by their indices in a
  given sequence, and sets of these elements.
  The tables are vectors of vectors (the rows of the table),
  so multiplication is just look up."
  (:require [clojure.set :refer [difference union subset?]]
            [orbit.core :refer [partial-orbit full-orbit]]
            [kigen.sgp :as sgp]
            [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]))

(defmacro at
  "Convenient accessing of matrix elements."
  [mt i j]
  `((~mt ~i) ~j))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  [xs mul]
  (let [vxs (vec xs)
        indx (zipmap vxs (range (count vxs)))]
    (vec (map
          (fn [x] (->> xs
                       (map #(mul % x)) ;left multiplication by x
                       (map indx) ;elt -> index
                       (vec)))
          xs))))

(defn elts
  "Returns the elements of the given multiplication table.
  The elements are just the set of indices from 0 to n-1."
  [mt]
  (i/int-set (range (count mt))))

(defn set-mul
  "Set-wise multiplication of subsets of a multab. For A and B it returns AB."
  [mt A B]
  (i/int-set (for [i A j B] (at mt i j))))

(defn index-period
  "The index-period pair of integers in a vector in a multiplication table."
  [mt x]
  (sgp/index-period x (fn [x y] (at mt x y))))

(defn newelements
  "For a subsemigroup S and a subset X in mt this returns the elements
  (SX union XS) setminus (S union X)."
  [mt S X]
  (if (subset? X S)
    (i/int-set)
    (let [T (i/union S X)]
      (i/difference  (i/union (set-mul mt T X)
                              (set-mul mt X T))
                     T))))

(defn closure
  "It calculates the closure of base with elements in the set exts."
  ([mt exts] (closure mt (i/int-set) exts))
  ([mt base exts]
   (letfn
       [(finished? [[_ exts]] (empty? exts))
        (extend [[base exts]]
          #{[(i/union base exts) (newelements mt base exts)]})]
     (first
      (partial-orbit [base exts] extend (fn [x] true) finished?)))))

;TODO this should be int-mapped as well
(defn in-closure?
  "Returns true if an element x is in the closure of sgp by gens"
  ([mt gens x] (in-closure? mt (i/int-set) gens x))
  ([mt sgp gens x]
   (letfn
       [(finished? [[sgp gens]]
          (or (contains? sgp x) (contains? gens x)))
        (extend [[sgp gens]]
          #{[(union sgp gens) (newelements mt sgp gens)]})]
     (some?
      (partial-orbit [sgp gens] extend (fn [x] true) finished?)))))

(defn min-extensions
  "Returns the minimal extensions (by new element) of closed subarray of
  multiplication table mt."
  [mt elts closedsub]
  (let [reducef (fn ([] #{})
                  ([acc x]
                   (conj acc
                         (closure mt closedsub (i/int-set [x])))))]
    (r/reduce reducef
              (r/remove closedsub elts))))

(defn subsgps
  "All subsemigroups of an abstract semigroup given by its multiplication
  table"
  [mt]
  (full-orbit [(i/int-set)]
              (partial min-extensions mt (elts mt))))
