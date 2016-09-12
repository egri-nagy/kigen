(ns kigen.chain
 (:require [kigen.orbit :as o]
           [clojure.set :refer [union]]))

;; search is done with a BFS
(defn chains
 "All chains between elements a and b in the given (explicit) cover relation."
  [a b cr]
  (letfn [(solution? [chain] (= b (last chain)))
          (extensions [chain]
            (when-not (solution? chain)
              (map (partial conj chain) (cr (last chain)))))]
    (filter solution? (o/bfs [[a]] extensions))))

(defn gaps
  "Returns all pairs of consecutive elements of the chain that are not in
    cover relation."
  [chain cr]
  (filter
   #(not (contains? (cr (first %)) (second %)))
   (partition 2 1 chain)))


(defn distance-calculator
  "For a Hasse-diagram/cover relation cr this perform a BFS and records the
    discovery times. Returns a map: element -> vector of discovery times"
  [e cr f]
  (loop [d 0
         dists {}
         frontier #{e}]
    (if (empty? frontier)
      (into {} (map (fn [[k v]] [k (apply max v)]) dists))
      (recur (inc d)
             (reduce #(update-in % [%2] conj d) dists frontier)
             (apply union (map cr frontier))))))

(defn max-distances
  "Returns a map of elements to their maximal distance
    (length of longest chain) from element e."
  [e cr]
  (distance-calculator e cr max))

(defn min-distances
  "Returns a map of elements to their minimal distance
     (length of shortest chain) from element e."
  [e cr]
  (distance-calculator e cr min))
