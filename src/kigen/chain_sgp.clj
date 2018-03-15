(ns kigen.chain-sgp
  "Chain semigroups based on a skeleton."
  (:require [clojure.set :refer [subset? superset?]]
            [orbit.core :refer [full-orbit]]
            [kigen.position :as pos]
            [kigen.chain :as chain]
            [kigen.poset :as p]
            [kigen.transf :as t]
            [kigen.skeleton :as sk]
            [clojure.math.combinatorics :refer [selections]]))

(defn max-chains
  "All maximal chains in Hasse diagram with top X."
  [X hd]
  (mapcat #(chain/chains % X hd) X))

(defn max-chains-sorted ;; Do we really need sorted?
  [sk] ;we need to compare them as vectors
  (sort (fn [x y] (compare (mapv vec x) (mapv vec y)))
        (max-chains (:stateset sk) (:supsethd sk))))

(defn on-hd
  "The result of acting by a transformation on superset Hasse diagram in a
  skeleton. The full state set and the singletons are put back."
  [hd X transf]
  (let [transformed (set (map #(t/act % transf) (keys hd)))
        elts (-> transformed
                 (into [X])
                 (into (map hash-set X)))]
    (p/cover-rel elts subset?)))

(defn gap?
  "Returns true if there is a gap between a and b in Hasse diagram hd, i.e.
  a is only transitively related to b."
  [a b hd]
  (not (contains? (hd a) b)))

(defn gaps
  "Returns all pairs in the reduced cover relation that are not related in
  the full relation."
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
  (let [rc (distinct (map #(t/act % t) c))
        rrc (if (= (last c) (last rc))
              rc
              (concat rc [(last c)]))]
    (reduce (fn [v x] (concat v
                              (get fllngs [(last v) x])
                              [x]))
            []
            rrc)))

(defn ->chain-transf
  "A transformation encoding the action of a transformation on all maximal
  chains."
  [{hd :supsethd X :stateset} chains t]
  (let [nhd  (on-hd hd X t)
        fngs (fillings (max-chains X nhd) hd)
        action (fn [c] (on-max-chain c t fngs))]
    ;(clojure.pprint/pprint nhd)
    ;(clojure.pprint/pprint fngs)
    (t/->transf chains action)))

(defn chain-transf->
  [{X :stateset} chains c-t]
  (let [v (mapv (comp first first) chains)
        m (zipmap (range) v)
        single-maps (map vector (range (count c-t)) c-t)
        r (set (for [[a b] single-maps] [(m a) (m b)]))] ;;get this from transf-conj
    (when (= (count r) (count X))
      (mapv second (sort r)))))

(defn chain-sgp-gens
  "Just a convenient function to map generators to chain semigroup generators."
  [gens]
  (let [skel (sk/skeleton gens)
        chains (max-chains-sorted skel)]
    (map (partial ->chain-transf skel chains)
         gens)))

(defn check-morphism
  [gens]
  (let [S (t/sgp-by-gens gens)
        sk (sk/skeleton gens)
        chains (max-chains-sorted sk)
        up (partial ->chain-transf sk chains)
        down (partial chain-transf-> sk chains)]
    (every?
     (fn [[u v]]
       (not= (t/mul u v)
             (down (t/mul (up u) (up v)))))
     (selections S 2))))
