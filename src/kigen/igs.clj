(ns kigen.igs
  "Independent sets of semigroups represented by multiplication tables."
  (:require [clojure.data.int-map :refer [int-set union difference]]
            [kigen.multab :as multab]))

(defn independent?
  "Decides whether the set A is an independent set in multiplication table mt."
  [mt A]
  (every?
   (fn [a]
     (not (multab/in-closure? mt (difference A (int-set [a])) a)))
   A))

(defn min-extensions
  "All independent sets that are minimal extensions of the independent set I.
  mt - multiplication table, elts - a subset of elements of the semigroup,
  I - the set to be extended"
  [mt elts I]
  (let [complement (difference elts I)]
    (filter (partial independent? mt)
            (map (partial conj I) complement))))
