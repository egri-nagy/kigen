;;TODO consider using immutable bitsets from https://github.com/clojure/data.int-map
(ns kigen.multab
  "Functions for dealing with abstract multiplication tables,
  i.e. multiplicative elements are represented by their indices in a
  given sequence."
  (:require [clojure.set :refer [difference union]]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]
            [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  ([xs] (multab xs pbr/mul))
  ([xs mul]
   (let [indices (pos/indexed (vec xs))]
     (vec (pmap
           (fn [x] (->> xs
                        (map #(mul % x)) ;left multiplication by x
                        (map #(pos/pos (partial = %) indices)) ; elt -> index
                        (vec)))
           xs)))))

;; getting the i,j entry of the matrix mt
(defmacro at [mt i j]
  `(nth (nth ~mt ~i) ~j))

;; the resulting set may not contain the spanning elements in general
(defn set-mul
  "Setwise multiplication of subsets of a multab."
  ([mt A] (set-mul mt A A))
  ([mt A B] (set (for [i A j B] (at mt i j)))))

(defn extend-by
  "Extends a closed sub-array by elements exts."
  [mt base exts]
  (let [u (union base exts)]
    (union (set-mul mt exts u) (set-mul mt base exts))))

(defn closure
  "Returns the smallest closed subarray that contains the elements elts.
  Alternatively, a closed subarray closedsub can be extended by some elements
  elts."
  ;([mt elts] (closure mt #{} (union (set-mul mt elts) (set elts))))
  ([mt closedsub elts]
   (loop [base closedsub exts (set elts)]
     (if (empty? exts)
       base
       (let [gens (union base exts)
             nbase (union gens (extend-by mt base exts))
             nexts (difference nbase gens)]
         (recur nbase nexts))))))

(defn min-extensions
  "Returns the minimal extensions (by new element) of closed subarray of
  multiplication table mt."
  [mt closedsub]
  (let [complement (difference (set (range (count mt))) closedsub)]
    (set (pmap #(closure mt closedsub [%]) complement))))

(defn subsgps
  "All subsemigroups of an abstract semigroup given by its multiplication
  table"
  [mt]
  (orbit/dfs [#{}] (partial min-extensions mt)))
