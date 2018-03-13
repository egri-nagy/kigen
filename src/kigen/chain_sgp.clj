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
  [hd fullhd]
  (into {} (for [g (gaps hd fullhd)]
             [g ((comp butlast rest first) ;choice is made here, we pick the 1st
                 (chain/chains (first g) (second g) fullhd))])))

(defn on-max-chain
  [c t fllngs]
  (let [rc (distinct (map #(t/act % t) c))
        rrc (concat rc [(last c)])]
    (println rrc)
    (reduce
     (fn [v x] (concat v (get fllngs [(last v) x]) [x]))
     []
     rrc)))
