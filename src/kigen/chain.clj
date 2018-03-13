(ns kigen.chain
  "Chains in partially ordered sets given by their Hasse diagrams.
  A Hasse diagram  is a hash-map from elements to the set of related elements."
 (:require [orbit.core :refer [tree-search]]
           [clojure.set :refer [union]]))

(defn chains
 "All chains between elements a and b in the given Hasse diagram."
  [a b hd]
  (letfn [(solution? [chain] (= b (last chain)))
          (extensions [chain] ;no heuristics applied, no candidate? function
            (when-not (solution? chain)
              (map (partial conj chain) (hd (last chain)))))]
    (tree-search [[a]] extensions solution?)))

(defn gaps
  "Returns all pairs of consecutive elements of the chain that are not in
    cover relation."
  [chain hd]
  (remove (fn [[a b]] (contains? (hd a) b))
          (partition 2 1 chain)))

(defn discovery-times
  "For a Hasse-diagram/cover relation hd this perform a search and records the
    discovery times. Returns a map: element -> vector of discovery times
  f: min or max"
  [e hd]
  (loop [d 0
         dists {}
         frontier #{e}]
    (if (empty? frontier)
      dists
      (recur (inc d)
             (reduce #(update-in % [%2] conj d) dists frontier)
             (apply union (map hd frontier))))))

(defn max-distances
  "Returns a map of elements to their maximal distance
    (length of longest chain) from element e."
  [e hd]
  (into {} (map (fn [[k v]] [k (apply max v)])
                (discovery-times e hd))))
