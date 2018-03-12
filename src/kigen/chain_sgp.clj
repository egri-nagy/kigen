(ns kigen.chain-sgp
  "Chain semigroups based on a skeleton."
  (:require [clojure.set :refer [subset? superset?]]
            [orbit.core :refer [full-orbit partial-orbit]]
            [orbit.action :refer [set-action right-actions]]
            [kigen.cayley-graph :refer [cayley-graph]]
            [kigen.scc :refer [scc]]
            [kigen.position :as pos]
            [kigen.poset :as p]
            [kigen.chain :as chain]
            [kigen.transf :as t]
            [kigen.skeleton :as sk]))

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
  (let [sk (sk/skeleton gens)
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
