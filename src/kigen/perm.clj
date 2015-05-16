;;permutations
(ns kigen.perm
  (:use [kigen.transf :only [transf?]]))

(defn perm?
  [pbr]
  (and (transf? pbr)
       (= (:cod pbr) (reduce into #{} (for [key (:dom pbr)] (pbr key))))))
