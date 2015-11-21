;;permutations
(ns kigen.perm
  (:use [kigen.transf :only [binrel-transf?]]))

;;pbr is a permutation if images of elements of the domain yield the codomain
(defn binrel-perm?
  [pbr]
  (and (binrel-transf? pbr)
       (= (:cod pbr) (reduce into #{} (for [key (:dom pbr)] (pbr key))))))
