(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [kigen.transf :as t]
        [kigen.poset :as p]
        [clojure.set :as set]))

;;finite sets represented as 1..n
(defn finite-set
  "Returns the integers 1..n, the canonical representation of finite sets."
  [n] (set (range 1 (inc n))))

(defn subduction?
  [P Q afs]
  (or (set/subset? P Q)
      (contains? (:solutions (o/orbit-graph Q
                                            afs
                                            #(<= (count P) (count %))
                                            #(set/subset? % P)))
                 P)))

;;TODO extract the pattern for any pre-order
(defn equivalent?
  [P Q afs]
  (and (subduction? P Q afs)
       (subduction? Q P afs)))



;; returns a predicate that decides subduction between equivalence classes
(defn class-subduction
  [gens]
  (fn [clA clB]
    (some #(subduction? (second %) (first %) (o/right-actions t/act gens))
          (for [P clA Q clB] [P Q]))))

;;surduction is subduction the other way around
(defn class-surduction
  [gens]
  (fn [clA clB]
    (some #(subduction? (first %) (second %) (o/right-actions t/act gens))
          (for [P clA Q clB] [P Q]))))

;; due to the rule that singleton sets should have height zero
;; we have to be a bit tricky and find the minimal classes of non-singletons
;; this is done by doing surduction and find the maximum
(defn calc-heights
  [eqvcls gens]
  (let [nonsingl-eqvcls (remove #(t/singleton? (first %)) eqvcls)
        sur-hd (p/cover-rel nonsingl-eqvcls (class-surduction gens))
        sub-hd (p/cover-rel nonsingl-eqvcls (class-subduction gens))
        minimals (filter #(empty? (sur-hd %)) nonsingl-eqvcls)
        height-tabs (map #(p/max-distances % sub-hd) minimals)]
    (into {} (map (fn [k] [k (inc (apply max (remove nil? (map
                                                           #(% k)
                                                           height-tabs))))])
                  (keys sur-hd)))))

(defn expand-set-keyed-map
  "Takes a map whose keys are sets and returns another map where each element
  of key set maps to the value of the original key."
  [m]
  (into {} (apply concat (for [k (keys m)]
                           (for [e k]
                             [e (m k)])))))

;; creates a big map of holding all the skeleton information
(defn skeleton
  [gens]
  (let [stateset (finite-set (t/transf-degree (first gens)))
        singletons (map hash-set stateset)
        afs (o/right-actions t/act gens)
        images (o/bfs [stateset] (action-function afs))
        c-g (o/cayley-graph images afs)
        sccs (o/scc images c-g)
        scc-heights (calc-heights sccs gens)
        scc-heights-full (conj scc-heights [(set singletons) 0])
        heights (expand-set-keyed-map scc-heights-full)
        extd (into images (for [x stateset] #{x}))]
    {:stateset stateset
     :singletons singletons
     :images images
     :extended extd
     :equivclasses sccs
     :heights heights
     :height (heights stateset)
     :subsethd (p/cover-rel extd set/subset?)
     :supsethd (p/cover-rel extd set/superset?)}))

(defn depth
  ([sk P] (inc (- (:height sk) ((:heights sk) P))))
  ([sk] (depth sk #{1}))) ;the depth of a singleton, we should have this one

(defn tile-chains
  [sk]
  (mapcat #(p/all-chains % (:supsethd sk)) (:singletons sk)))

(defn chain-act [chain t]
  (distinct (map #(t/act % t) chain)))

(defn height [sk P] ((:heights sk) P))

(defn positioned
  [sk tc]
  (let [height ((:heights sk) (:stateset sk))
        ptc (vec (map (fn [x] :*) (range height)))]
    (reduce #(assoc % ((:heights sk) %2)  %2) ptc tc)))

(defn display
  [skeleton]
  (println "#states" (count (:stateset skeleton)))
  (println "#images" (count (:images skeleton)))
  (println "#equivclasses" (count (:equivclasses skeleton)))
  (println "#extd" (count (:extended skeleton))))
