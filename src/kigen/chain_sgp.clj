(ns kigen.chain-sgp
  "Chain semigroups based on a skeleton."
  (:require [clojure.set :refer [subset?]]
            [kigen.semigroup.sgp :refer [sgp-by-gens]]
            [kigen.chain :as chain]
            [kigen.poset :as poset]
            [kigen.diagram.transf :as t]
            [kigen.diagram.transf-conj :as t-conj]
            [kigen.skeleton :as sk]
            [clojure.math.combinatorics :refer [selections]]))

(defn max-chains
  "All maximal chains in an inclusion Hasse diagram hd with top set X."
  [X hd]
  (mapcat #(chain/chains % X hd)
          X))

(defn on-hd
  "The result of acting by a transformation on superset Hasse diagram in a
  skeleton. The full state set and the singletons are put back."
  [hd X transf]
  (let [transformed (set (map #(t/act % transf) (keys hd)))
        elts (-> transformed
                 (into [X])
                 (into (map hash-set X)))]
    (poset/cover-rel elts subset?)))

(defn gap?
  "Returns true if there is a gap between a and b in Hasse diagram hd, i.e.
  a is only transitively related to b."
  [a b hd]
  (not (contains? (hd a) b)))

(defn gaps
  "Returns all pairs in the reduced cover relation that are not related in
  the full relation. Chains have to be given since BECKS produces examples
  where [0 1 3 3] where filling the gaps in the reduced hd is not enough!"
  [chains fullhd]
  (set
   (filter (fn [[a b]] (gap? a b fullhd))
           (mapcat (partial partition 2 1) chains))))

(defn fillings
  "A map that contains filling for gaps in a reduced Hasse diagrams. Used
  for finding dominating chains."
  [chains fullhd]
  (into {} (for [g (gaps chains fullhd)]
             [g ((comp butlast rest first) ;choice is made here, we pick the 1st
                 (chain/chains (first g) (second g) fullhd))])))

(defn on-max-chain
  "Acting on  maximal chain. Finding a dominating chain by using a fillings
  table."
  [c t fllngs]
  (let [ct (distinct (map #(t/act % t) c)) ; hitting the chain with t
        tct (if (= (last c) (last ct)) ; putting beck the top if not there
              ct
              (concat ct [(last c)]))]
    (reduce (fn [v x]
              ; chain so far + filling between chain and next item + next item
              (concat v
                      (get fllngs [(last v) x]) ; nil for no gap
                      [x]))
            []
            tct)))

(defn ->chain-transf
  "A transformation encoding the action of a transformation on all maximal
  chains."
  [{hd :supsethd X :stateset} chains t]
  (let [nhd  (on-hd hd X t)
        fngs (fillings (max-chains X nhd) hd)
        action (fn [c] (on-max-chain c t fngs))]
    (t/->transf chains action)))

(defn chain-transf->
  "Taking a lift (a transformation of the set of chains) it gives back the
  original transformation."
  [{X :stateset} chains c-t]
  (let [v (mapv (comp first first) chains)
        m (zipmap (range) v)
        maps (t-conj/single-maps c-t)
        r (set (for [[a b] maps] [(m a) (m b)]))]
    (when (= (count r) (count X))
      (mapv second (sort r)))))

(defn chain-sgp-gens
  "Just a convenient function to map generators to chain semigroup generators."
  [gens]
  (let [skel (sk/skeleton gens)
        chains (max-chains (:stateset skel) (:supsethd skel))]
    (map (partial ->chain-transf skel chains)
         gens)))

(defn check-morphism
  "Takes a generator set of transformations, produces a chain semigroup and
  checks the morphic relation by checking all products."
  [gens]
  (let [S (sgp-by-gens gens t/mul)
        sk (sk/skeleton gens)
        chains (max-chains (:stateset sk) (:supsethd sk))
        up (partial ->chain-transf sk chains)
        down (partial chain-transf-> sk chains)]
    (every?
     (fn [[u v]]
       (not= (t/mul u v)
             (down (t/mul (up u) (up v)))))
     (selections S 2))))
