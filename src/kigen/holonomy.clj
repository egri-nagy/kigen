(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [kigen.transf :as t]
        [kigen.poset :as p]
        [clojure.set :as set]))

;;finite sets represented as 1..n
(defn finite-set
  "Returns the set of integers 1..n, the canonical representation of finite sets."
  [n] (set (range 1 (inc n))))

(defn subduction?
  [P Q gens]
  (or (set/subset? P Q)
      (contains? (o/orbit Q gens t/act) P)))

;;TODO extract the pattern for any preorder
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

;; due to the rule that singleton sets should have height zero
;; we have to be a bit tricky and find the minimal classes of non-singletons
;; this is done by doing surduction and find the maximum
(defn calc-heights
  [eqvcls gens]
  (let [nonsingl-eqvcls (remove #(t/singleton? (first %)) eqvcls)
        sur-hd (p/hasse-diagram nonsingl-eqvcls (class-surduction gens))
        minimals (filter #(empty? (sur-hd %)) nonsingl-eqvcls)
        sub-hd (p/hasse-diagram nonsingl-eqvcls (class-subduction gens))
        height-tabs (map #(p/max-distances % sub-hd) minimals)]
    (into {} (map (fn [k] [k (inc (apply max (map #(% k) height-tabs)))])
                  (keys sur-hd)))))

(defn invert-set-keyed-map
  [m]
  (into {} (apply concat (for [k (keys m)] (for [p k] [p (m k)])))))
;; creates a big map of holding all the skeleton information
(defn skeleton
  [gens]
  (let [stateset (finite-set (t/transf-degree (first gens)))
        images (o/orbit stateset gens t/act)
        c-g (o/cayley-graph images (for [x gens] #(t/act % x)))
        sccs (o/scc images c-g)
        heights (calc-heights sccs gens)
        extd (into images (for [x stateset] #{x}))]
    {:stateset stateset
     :images images
     :equivclasses sccs
     :heights (invert-set-keyed-map heights)
     :subsethd (p/hasse-diagram images set/subset?)
     :supsethd (p/hasse-diagram images set/superset?)
     }))
