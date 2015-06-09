(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [clojure.set :as set]))

(defn finite-set
  [n]
  (set (range 1 (inc n))))

(defn subduction
  [P Q gens action]
  (or (set/subset? P Q)
      (contains? (o/orbit Q gens action) P)))

(defn equivalent?
  [P Q gens action]
  (and (subduction P Q gens action)
       (subduction Q P gens action)))
