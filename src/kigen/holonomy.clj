(ns kigen.holonomy
  (:use [kigen.orbit :as o]
        [kigen.transf :as t]
        [kigen.poset :as p]
        [clojure.set :as set]))

(defn finite-set
  "Returns the set of integers 1..n, canonical representation of finite sets."
  [n] (set (range 1 (inc n))))

(defn subduction-function
  "P subduces Q if Q can be transformed by the actions to be a superset of P.
  P,Q: finite sets; as: transformations acting on the right"
  [as]
  (fn [P Q]
    (or (set/subset? P Q)
        (not-empty (:solutions (o/controlled-bfs Q
                                                 (o/set-action as)
                                                 #(<= (count P) (count %))
                                                 #(set/superset? % P)))))))

;; due to the rule that singleton sets should have height zero
;; we have to be a bit tricky and find the minimal classes of non-singletons
;; this is done by doing surduction and find the maximum
(defn calc-heights
  [eqvcls subduction?]
  (let [nonsingl-eqvcls (remove #(t/singleton? (first %)) eqvcls)
        class-subduction? (fn [clA clB]
                            (subduction? (first clA) (first clB)))
        class-surduction? (p/inverse class-subduction?)
        sur-hd (p/cover-rel nonsingl-eqvcls class-surduction?)
        sub-hd (p/cover-rel nonsingl-eqvcls class-subduction?)
        minimals (filter #(empty? (sur-hd %)) nonsingl-eqvcls)
        height-tabs (map #(p/max-distances % sub-hd) minimals)
        max-dist (fn [k] (apply max (remove nil? (map ;across all height-tabs
                                                  #(% k)
                                                  height-tabs))))]
    (into {} (map #(vector % (inc (max-dist %))) (keys sur-hd)))))

(defn expand-set-keyed-map
  "Takes a map whose keys are sets and returns another map where each element
  of key set maps to the value of the original key."
  [m]
  (into {} (apply concat (map
                          (fn [key] (let [val (m key)] (for [e key] [e val])))
                          (keys m)))))

(defn skeleton
  "Creates a big map of holding all the skeleton information for the
  transformation semigroup generated by gens."
  [gens]
  (let [r-a-gens (o/right-actions t/act gens) ; right action generators
        subduction? (subduction-function r-a-gens)
        stateset (finite-set (t/transf-degree (first gens)))
        singletons (set (map hash-set stateset))
        images (o/bfs [stateset] (set-action r-a-gens))
        extd (into images singletons)
        c-g (o/cayley-graph images r-a-gens)
        sccs (o/scc images c-g)
        scc-heights (calc-heights sccs subduction?)
        scc-heights-full (conj scc-heights [(set singletons) 0])
        heights (expand-set-keyed-map scc-heights-full)]
    {:stateset stateset
     :singletons singletons
     :images images
     :extended extd
     :equivclasses sccs
     :heights heights
     :height (heights stateset)
     :supsethd (p/cover-rel extd set/subset?)}))

(defn depth
  ([sk P] (inc (- (:height sk) ((:heights sk) P))))
  ([sk] (depth sk #{1}))) ;the depth of a singleton, we should have this one

(defn tile-chains-from
  "Ascending chains from set P in skeleton sk."
  [sk P]
  (p/all-chains P (:supsethd sk)))

(defn dominating-chains
  "All dominating chains in skeleton sk for a chain."
  [sk chain]
  (let [sets (set chain)
        singleton (first (filter #(= 1 (count %)) sets))]
    (if-not (nil? singleton)
      (filter #(subset? sets (set %))
              (tile-chains-from sk singleton)))))

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
