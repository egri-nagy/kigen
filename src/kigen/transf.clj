(ns kigen.transf
  (:require [kigen.pbr :as pbr])
  (:use [kigen.perm] :only [symmetric-gens]))

(defn transf->pbr
  "Creates a partitioned binary relation from a transformation
  given by the list of images.
  Transformations index point from 1, unlike the vector indices."
  [imagelist]
  (let [emptyset #{}
        n (count imagelist)
        pbr {:dom (set (range 1 (inc n)))
             :cod (set (range (inc n) (inc (* 2 n))))}
        edges (into {} (map
                        #(vector % (set [(+ n (nth imagelist (dec %)))]))
                        (:dom pbr)))
        non-edges (into {} (map
                            #(vector % emptyset)
                            (:cod pbr)))]
    (reduce into pbr (concat [edges non-edges]))))

(defn full-ts-gens
  "Generators of the full transformation semigroup of degree n."
  [n]
  (let [collapse (concat [1 1] (range 3 (inc n)))]
    (concat (symmetric-gens n) [(transf->pbr collapse)])))

;;acting as pbr, then shift back the resulting set
(defn act
  [points t]
  (set (map #(- % (count (:dom t))) (pbr/act points t))))
