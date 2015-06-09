(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [kigen.transf :as t]
        [clojure.set :as set]))

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

(defn equivalence-classes
  [base gens]
  (let [images (o/orbit base gens t/act)
        c-g (o/cayley-graph images (for [x gens] #(t/act % x)))]
    (o/scc images c-g)))

(defn class-subduction
  [gens]
  (fn [clA clB]
    (some #(subduction (second %) (first %) gens)
          (for [P clA Q clB] [P Q]))))
