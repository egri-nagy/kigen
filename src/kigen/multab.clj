;;TODO consider using immutable bitsets from https://github.com/clojure/data.int-map
(ns kigen.multab
  "Functions for dealing with abstract multiplication tables,
  i.e. multiplicative elements are represented by their indices in a
  given sequence, and sets of these elements."
  (:require [clojure.set :refer [difference union subset?]]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]
            [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
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
  "Returns the elements of the given multiplication table."
  [mt]
  (set (range (count mt))))

(defn set-mul
  "Setwise multiplication of subsets of a multab. For A and B it returns AB."
  [mt A B]
  (set (for [i A j B] ((mt i) j))))

(defn newelements
  "For a subsemigroup S in mt this returns the elements
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
   (if (empty? exts)
     base
     (recur mt (union base exts) (newelements mt base exts)))))

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
