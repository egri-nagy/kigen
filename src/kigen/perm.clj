;;permutations

(ns kigen.perm
  (:require [kigen.pbr :as pbr]
            [kigen.transf :refer [binrel-transf?]]))

;;pbr is a permutation if images of elements of the domain yield the codomain
(defn binrel-perm?
  [pbr]
  (and (binrel-transf? pbr)
       (= (:cod pbr) (reduce into #{} (for [key (:dom pbr)] (pbr key))))))

(defn conjugate
  "the conjugate of x by p, i.e. p^{-1}xp"
  [x p]
  (pbr/mul (pbr/mul (pbr/flip p) x) p))
