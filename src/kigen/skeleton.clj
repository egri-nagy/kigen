(ns kigen.skeleton
  "Skeleton of a transformation semigroup given by a set of  generators."
  (:require [clojure.set :refer [subset? superset?]]
            [orbit.core :refer [full-orbit partial-orbit]]
            [orbit.action :refer [set-action right-actions]]
            [kigen.cayley-graph :refer [cayley-graph]]
            [kigen.scc :refer [scc]]
            [kigen.pos :as pos]
            [kigen.poset :as p]
            [kigen.chain :as chain]
            [kigen.transf :as t]))

(defn finite-set
  "Returns the set of integers 0..n-1, canonical representation of an n-element
   finite set."
  [n]
  (set (range n)))

(defn subduction-function
  "P subduces Q if Q can be transformed to Qt by some action to be a superset
  of P. P,Q: finite sets; actions: transformations acting on the right"
  [actions]
  (fn [P Q]
    (or (subset? P Q)
        (not (nil? (partial-orbit Q
                                  (set-action actions)
                                  (fn [Qt] (<= (count P) (count Qt)))
                                  (fn [Qt] (superset? Qt P))))))))

(defn calc-heights
  [eqvcls subduction?]
  (let [singletons (filter #(t/singleton? (first %)) eqvcls)
        ;;singletons should have height zero, hence to modified subduction?
        class-subduction? (fn [clA clB]
                            (when-not (t/singleton? (first clB))
                                      (subduction? (first clA) (first clB))))
        sub-hd (p/cover-rel eqvcls class-subduction?)
        height-tabs (map #(chain/max-distances % sub-hd) singletons)]
    (apply merge-with max height-tabs)))

(defn expand-set-keyed-map
  "Takes a map whose keys are sets and returns another map where each element
  of key set maps to the value of the original key."
  [m]
  (into {} (mapcat
            (fn [key] (let [val (m key)] (for [e key] [e val])))
            (keys m))))

(defn skeleton
  "Creates a big map of holding all the skeleton information for the
  transformation semigroup generated by gens."
  [gens]
  (let [actions (right-actions t/act gens)
        subduction? (subduction-function actions)
        stateset (finite-set (count (first gens)))
        singletons (set (map hash-set stateset))
        images (full-orbit [stateset] (set-action actions))
        extd (into images singletons)
        sccs (scc images (cayley-graph images actions))
        scc-heights (calc-heights sccs subduction?)
        heights (expand-set-keyed-map scc-heights)]
    {:stateset stateset
     :singletons singletons
     :images images
     :extended extd
     :equivclasses sccs
     :heights heights
     :height (heights stateset)
     :supsethd (p/cover-rel extd subset?)}))

(defn depth
  ([sk P] (inc (- (:height sk) ((:heights sk) P))))
  ([sk] (depth sk #{1}))) ;the depth of a singleton, we should have this one

(defn display
  [skeleton]
  (println "#states" (count (:stateset skeleton)))
  (println "#images" (count (:images skeleton)))
  (println "#equivclasses" (count (:equivclasses skeleton)))
  (println "#extd" (count (:extended skeleton))))