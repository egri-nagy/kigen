;;permutations
(ns kigen.perm
  (:use [kigen.transf :only [transf?]]))

;;pbr is a permutation if images of elements of the domain yield the codomain
(defn perm?
  [pbr]
  (and (transf? pbr)
       (= (:cod pbr) (reduce into #{} (for [key (:dom pbr)] (pbr key))))))
