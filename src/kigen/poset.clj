;; partially ordered sets
(ns kigen.poset
  (:use [clojure.set :only [union]]))

(declare hasse-diagram) ; calculates the Hasse-diagram graph of a relation

;; Not the most clever algorithm: it assumes that anything related is a cover
;; then gets rid of it if proven not to be a cover
;; elts - set of elements
;; rel - a partial order relation predicate
(defn hasse-diagram
  "Creates the Hasse-diagram of a partial order as a graph
  (map: element -> set of covers), given by the elements and a predicate."
  [elts rel]
  (letfn [(recalc-covers [covers newval] ;recalculate the set of covers
            (if (some #(rel newval %) covers) ;nothing to do newval is below
              covers
              (conj (set (filter #(not (rel % newval)) covers)) newval)))
          (insert [hd e] ;insert an element into the graph by updating covers
            (let [candidates (filter #(and (not (= e %))
                                           (rel e %))
                                     (keys hd))]
              (reduce #(assoc % %2 (recalc-covers (%  %2) e))
                      hd
                      candidates)))]
    ;starting with the empty covering sets we insert all elements
    (reduce insert
            (into {} (for [e elts] [e #{}]))
            elts)))

;; adding all successors of the last element of the chain
;; returns a seq of these extended chains
(defn chain-extensions
  [chain hd]
  (map (partial conj chain) (hd (last chain))))

;; set
;; hd Hasse-diagram
(defn all-chains
  [e hd]
  (letfn [(f [x] (apply concat (map #(chain-extensions % hd) x)))]
    (last (take-while #(not (empty? %)) (iterate f [[e]])))))

;; maximal distances from an element in a Hasse-diagram (longest chain)
(defn max-distances-from
  [e hd]
  (loop [d 0
         dists {}
         frontier #{e}]
    (if (empty? frontier)
      dists
      (recur (inc d)
             (reduce #(assoc % %2 d) dists frontier)
             (apply union (map hd frontier))))))
