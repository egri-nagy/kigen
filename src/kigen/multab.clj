(ns kigen.multab
  "Functions for dealing with abstract multiplication tables of semigroups.
  The multiplicative elements are represented by their indices in a
  given sequence. The tables are vectors of vectors (the rows of the table),
  so multiplication is just a constant-time look up.
  Functionality for multiplying subsets of elements and computing closures
  and thus enumerating subsemigroups. The subsemigroups can be stored in
  efficient int-sets."
  (:require [clojure.set :refer [difference union subset?]]
            [orbit.core :refer [partial-orbit full-orbit pfull-orbit]]
            [kigen.sgp :as sgp]
            [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]))

(defmacro at
  "Convenient accessing of matrix elements by their indices."
  [mt i j]
  `((~mt ~i) ~j))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul.
  The order of elements in xs is preserved and used to define indices."
  [xs mul]
  (let [vxs (vec xs)
        indx (zipmap vxs (range (count vxs)))]
    (mapv
     (fn [x] (->> xs
                  (map #(mul % x)) ;left multiplication by x
                  (map indx) ;elt -> index
                  (vec)))
     xs)))

(defn elts
  "Returns the elements of the given multiplication table.
  The elements are just the set of indices from 0 to n-1 in an int-set."
  [mt]
  (i/int-set (range (count mt))))

(defn index-period
  "The index-period pair of integers in a vector in a multiplication table."
  [mt x]
  (sgp/index-period x (fn [x y] (at mt x y))))

(defn set-mul
  "Set-wise multiplication of subsets of a multab. For A and B it returns AB."
  [mt A B]
  (i/int-set (for [i A j B] (at mt i j))))

(defn newelements
  "For a subsemigroup S and a subset X in mt this returns the elements
  ((S U X)X U X(S U X)) setminus (S U X)."
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
      (partial-orbit [base exts] extend (constantly true) finished?)))))

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
  (let [reducef (fn
                  ([] #{})
                  ([acc x]
                   (conj acc
                         (closure mt closedsub (i/int-set [x])))))]
    (r/reduce reducef
              (r/remove closedsub elts))))

(defn subsgps
  "All subsemigroups of an abstract semigroup given by its multiplication
  table."
  [mt]
  (full-orbit [(i/int-set)]
              (partial min-extensions mt (elts mt))))

(defn psubsgps
  "All subsemigroups of an abstract semigroup given by its multiplication
  table computed in parallel."
  [mt]
  (pfull-orbit [(i/int-set)]
               (partial min-extensions mt (elts mt))))
