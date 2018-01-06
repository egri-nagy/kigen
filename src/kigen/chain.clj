(ns kigen.chain
  "Chains in partially ordered sets."
 (:require [orbit.core :refer [tree-search]]
           [clojure.set :refer [union]]))

;; search is done with a FULL-ORBIT
(defn chains
 "All chains between elements a and b in the given (explicit) cover relation."
  [a b cr]
  (letfn [(solution? [chain] (= b (last chain)))
          (extensions [chain]
            (when-not (solution? chain)
              (map (partial conj chain) (cr (last chain)))))]
    (tree-search [[a]] extensions solution?)))

(defn gaps
  "Returns all pairs of consecutive elements of the chain that are not in
    cover relation."
  [chain cr]
  (filter
   #(not (contains? (cr (first %)) (second %)))
   (partition 2 1 chain)))


(defn discovery-times
  "For a Hasse-diagram/cover relation cr this perform a search and records the
    discovery times. Returns a map: element -> vector of discovery times
  f: min or max"
  [e cr]
  (loop [d 0
         dists {}
         frontier #{e}]
    (if (empty? frontier)
      dists
      (recur (inc d)
             (reduce #(update-in % [%2] conj d) dists frontier)
             (apply union (map cr frontier))))))

(defn max-distances
  "Returns a map of elements to their maximal distance
    (length of longest chain) from element e."
  [e cr]
  (into {} (map (fn [[k v]] [k (apply max v)])
                (discovery-times e cr))))
