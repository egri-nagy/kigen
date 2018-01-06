(ns kigen.holonomy
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
        height-tabs (map #(chain/max-distances % sub-hd) minimals)
        max-dist (fn [k] (apply max (remove nil? (map ;across all height-tabs
                                                  #(% k)
                                                  height-tabs))))]
    (into {} (map #(vector % (inc (max-dist %))) (keys sur-hd)))))

(defn elts->coll [xs]
  (reduce (fn [m x] (assoc m x set))
          {}
          xs))

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
        c-g (cayley-graph images actions)
        sccs (scc images c-g)
        scc-heights (calc-heights sccs subduction?)
        scc-heights-full (conj scc-heights [(set singletons) 0])
        heights (expand-set-keyed-map scc-heights-full)] ; composition of maps would be nicer
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

(defn tile-chains-from
  "Ascending chains from set P in skeleton sk."
  [sk P]
  (chain/chains P (:stateset sk) (:supsethd sk)))

(defn tile-chains
  "All maximal chains in skeleton sk."
  [sk]
  (mapcat #(tile-chains-from sk %) (:singletons sk)))

(defn dominating-chains
  "All dominating chains in skeleton sk for a chain."
  [sk chain]
  (let [sets (set chain) ; representing the chain as a set
        singleton (first (filter #(= 1 (count %)) chain))] ;finding its singleton
    (if-not (nil? singleton)
      (filter #(subset? sets (set %))
              (tile-chains-from sk singleton)))))

(defn on-chain
  "Acting on a chain by a transformation on the right."
  [chain transf]
  (distinct (map #(t/act % transf) chain)))

(defn in-chain-comparator
  "Comparator for chain elements, works only for total orders."
  [x y]
  (cond (subset? x y) 1
        (superset? x y) -1
        (= x y) 0))

(defn chain-sgp 
  "Returns the encoded generators of the full unrestricted chain semigroup."
  [gens]
  (let [sk (skeleton gens)
        sgpact (fn [chain] (set (map #(on-chain chain %) gens)))
        maxchains (tile-chains sk)
        chains (vec (full-orbit maxchains sgpact))
        posf (fn [dc] (pos/position #( = (set dc) (set %)) chains))]
    (map
     (fn [t] (let [imgs (map #(on-chain % t) chains)]
               (map posf imgs)))
     gens)))

(defn chop [l] (drop 1 (take (dec (count l)) l)))

(defn chain-transf
  [sk t]
  (let [tilechains (tile-chains sk)
        transfdchains (map #(on-chain % t) tilechains)
        dchains (map #(conj (vec %) (:stateset sk)) (distinct transfdchains)); don't conj when it's there
        gaps (distinct (mapcat #(chain/gaps % (:supsethd sk)) dchains)) ;;TODO think about context first for args, so we can partial
        fillingf (fn [pair] (chain/chains (first pair) (second pair) (:supsethd sk)))
        fillings (into {} (map #(vector % (fillingf %)) gaps)); gap (pair) -> list of possible fillings
        ;;now the indexing part
        indxd (vec tilechains)
        posf (fn [dc] (pos/position #( = (set dc) (set %)) indxd))]

    fillings)) ;just temporarily

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
