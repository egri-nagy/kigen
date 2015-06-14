(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [kigen.transf :as t]
        [kigen.poset :as p]
        [clojure.set :as set]))

;;finite sets represented as 1..n
(defn finite-set [n] (set (range 1 (inc n))))

(defn subduction?
  [P Q gens]
  (or (set/subset? P Q)
      (contains? (o/orbit Q gens t/act) P)))

(defn equivalent?
  [P Q gens]
  (and (subduction? P Q gens t/act)
       (subduction? Q P gens t/act)))

;; returns a predicate that decides subduction between equivalence classes
(defn class-subduction
  [gens]
  (fn [clA clB]
    (some #(subduction? (second %) (first %) gens)
          (for [P clA Q clB] [P Q]))))

;;surduction is subduction the other way around
(defn class-surduction
  [gens]
  (fn [clA clB]
    (some #(subduction? (first %) (second %) gens)
          (for [P clA Q clB] [P Q]))))

;; creates a big map of holding all the skeleton information
(defn skeleton
  [gens]
  (let [stateset (finite-set (t/transf-degree (first gens)))
        images (o/orbit stateset gens t/act)
        c-g (o/cayley-graph images (for [x gens] #(t/act % x)))
        sccs (o/scc images c-g)
        nonsingleton-sccs (remove #(t/singleton? (first %)) sccs)
        surduction-hd (p/hasse-diagram nonsingleton-sccs class-surduction)
        minimals (filter #()  nonsingleton-sccs)
        heights ()]
    {:stateset stateset
     :images images
     :equivclasses sccs
     }))
