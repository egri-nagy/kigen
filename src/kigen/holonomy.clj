(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [clojure.set :as set]))

(defn subduction
  [P Q gens action]
  (or (set/subset? P Q)
      (contains? (o/orbit Q gens action) P)))
