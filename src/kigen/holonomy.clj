(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [clojure.set :as set]
        [kigen.transf :as t]))

(defn finite-set
  [n]
  (set (range 1 (inc n))))

(defn subduction
  [P Q gens]
  (or (set/subset? P Q)
      (contains? (o/orbit Q gens t/act) P)))

(defn equivalent?
  [P Q gens]
  (and (subduction P Q gens t/act)
       (subduction Q P gens t/act)))
