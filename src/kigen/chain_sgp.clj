(ns kigen.chain-sgp
  "Chain semigroups based on a skeleton."
  (:require [clojure.set :refer [subset? superset?]]
            [orbit.core :refer [full-orbit]]
            [kigen.position :as pos]
            [kigen.chain :as chain]
            [kigen.poset :as p]
            [kigen.transf :as t]
            [kigen.skeleton :as sk]))

(defn max-chains
  "All maximal chains in skeleton sk."
  [{singletons :singletons
    stateset :stateset
    hd :supsethd}]
  (mapcat #(chain/chains % stateset hd) singletons))


(defn on-hd
  "The result of acting by a transformation on superset Hasse diagram in a
  skeleton. The full state set and the singletons are put back."
  [hd X transf]
  (let [transformed (set (map #(t/act % transf) (keys hd)))
        elts (-> transformed
                 (into [X])
                 (into (map hash-set X)))]
    (p/cover-rel elts subset?)))

(defn gaps
  "Returns all pairs in the reduced cover relation that are not related in
  the full relation."
  [hd fullhd]
  (remove (fn [[a b]] (contains? (fullhd a) b))
          (for [k (keys hd)
                v (hd k)]
            [k v])))

(defn fillings
  "A map that contains filling for gaps in a reduced Hasse diagrams. Used
  for finding dominating chains."
  [hd fullhd]
  (into {} (for [g (gaps hd fullhd)]
             [g ((comp butlast rest first) ;choice is made here, we pick the 1st
                 (chain/chains (first g) (second g) fullhd))])))

(defn on-max-chain
  "Acting on  maximal chain. Finding a dominating chain by using a fillings
  table."
  [c t fllngs]
  (let [rc (distinct (map #(t/act % t) c))
        rrc (if (= (last c) (last rc))
              rc
              (concat rc [(last c)]))]
    (reduce (fn [v x] (concat v
                              (get fllngs [(last v) x])
                              [x]))
            []
            rrc)))

(defn ->transf
  "A transformation encoding the action of a transformation on all maximal
  chains."
  [sk t]
  (let [m-chains (max-chains sk)
        sorted-max-chains  (sort (fn [x y] (compare (mapv vec x) (mapv vec y)))
                                 m-chains);compare as vectors
        nhd  (on-hd (sk :supsethd) (sk :stateset) t)
        fngs (fillings nhd (:supsethd sk))]
    (mapv (fn [c] (pos/index sorted-max-chains
                             (on-max-chain c t fngs)))
          sorted-max-chains)))

(defn chain-sgp-gens
  "Just a convenient function to map generators to chain semigroup generators."
  [gens]
  (map (partial ->transf (sk/skeleton gens))
       gens))
