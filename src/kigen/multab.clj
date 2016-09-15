;;TODO consider using immutable bitsets from https://github.com/clojure/data.int-map
(ns kigen.multab
  "Functions for dealing with abstract multiplication tables,
  i.e. multiplicative elements are represented by their indices in a
  given sequence."
  (:require [clojure.set :refer [difference union subset?]]
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

(defn set-mul
  "Setwise multiplication of subsets of a multab."
  ([mt A] (set-mul mt A A))
  ([mt A B] (set (for [i A j B] (at mt i j)))))

(defn newelements [mt S X]
  "For a subsemigroup S in mt this returns the elements
  (SX union XS) setminus (S union X)."
  (if (subset? X S)
    #{}
    (let [T (union S X)]
      (remove T (union (set-mul mt T X)
                       (set-mul mt X T))))))

(defn closure
  [mt closedsub elts]
  (loop [base closedsub, exts (set elts)]
    (if (empty? exts)
      base
      (recur (union base exts) (newelements mt base exts)))))

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
