;;TODO consider using immutable bitsets from https://github.com/clojure/data.int-map
(ns kigen.multab
  "Functions for dealing with abstract multiplication tables,
  i.e. multiplicative elements are represented by their indices in a
  given sequence, and sets of these elements.
  The tables are vectors of vectors (the rows of the table),
  so multiplication is just look up."
  (:require [clojure.set :refer [difference union subset?]]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]
            [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul.
  The default is PBR multiplication. Computation is done by rows in parallel."
  ([xs] (multab xs pbr/mul))
  ([xs mul]
   (let [vxs (vec xs)]
     (vec (pmap
           (fn [x] (->> xs
                        (map #(mul % x)) ;left multiplication by x
                        (map #(pos/index vxs %)) ;elt -> index
                        (vec)))
           xs)))))

(defn elts
  "Returns the elements of the given multiplication table.
  The elements are just the set of indices from 0 to n-1."
  [mt]
  (set (range (count mt))))

(defn set-mul
  "Setwise multiplication of subsets of a multab. For A and B it returns AB."
  [mt A B]
  (set (for [i A j B] ((mt i) j))))

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
          (or (empty? gens) (contains? sgp x) (contains? gens x)))
        (extend [[sgp gens]]
          #{[(set  (union sgp gens)) (set (newelements mt sgp gens))]})]
     (nil?
      (orbit/first-solution-single [sgp gens] extend (fn [x] true) finished?)))))

(defn min-extensions
  "Returns the minimal extensions (by new element) of closed subarray of
  multiplication table mt."
  [mt closedsub]
  (let [complement (difference (elts mt) closedsub)]
    (set (map #(closure mt closedsub #{%}) complement))))

(defn subsgps
  "All subsemigroups of an abstract semigroup given by its multiplication
  table"
  [mt]
  (orbit/full-orbit-single [#{}] (partial min-extensions mt)))
