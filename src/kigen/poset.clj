;; partially ordered sets
;; ways of defining binary relations:
;; 1. implicit, elements and a relation function; x rel y
;; 2. explicit, a map: element x -> set of related elements,
;;    i.e. all y such that x rel y
(ns kigen.poset
  (:use [clojure.set :only [union]]))

(declare rel ;explicit relation
         cover-rel ;calculates the covering relation of a relation
         all-chains ;all chains from an element
         max-distances) ;maximal distances from an element

(defn rel
  "Makes an implicit relation explicit. Works for finite set of elements."
  [elts rel?]
  (into {} (map
            #(vec [% (set (filter (fn [x] (rel? % x)) elts))])
            elts)))

(defn inverse
  "Given an implicit relation by function rel? this returns the function
  for the inverse relation."
  [rel?]
  (fn [x y] (rel? y x))) ; just swapping arguments

(defn equivalent
  [preorder-rel?]
  (fn [x y] (and (preorder-rel? x y)
                 (preorder-rel? y x))))


;; A cubic algorithm for finding covers for a binary relation.
;; It assumes that anything related is a cover
;; then gets rid of an element if it is proven not to be a cover
;; elts - set of elements
;; rel? - a partial order relation predicate, (rel? a b) Is a below b?
(defn cover-rel
  "The covering relation of a partial order as a graph
  (map: element -> set of covers), given by the elements and a predicate."
  [elts rel?]
  (let [emptytab (into {} (for [e elts] [e #{}]))
        recalc-covers (fn [covers newval] ;recalculate the set of covers
                        (if (some #(rel? % newval) covers)
                          covers ;newval is below some of covers, no change
                          (conj
                           (set (filter #(not (rel? newval %)) covers))
                           newval))) ;the subset of covers not below and newval
        insert (fn [cr e] ;insert an element into the graph by updating covers
                 (let [xs (filter #(and (not (= e %)) (rel? % e)) elts)]
                   (reduce #(assoc % %2 (recalc-covers (% %2) e)) cr xs)))]
    (reduce insert emptytab elts)))

(defn all-chains
  "All chains from element e in the cover-rel cr."
  [e cr]
  (letfn [(chain-extensions
            [chain]
            (map (partial conj chain) (cr (last chain))))]
   ;; (loop [stack [[e]] chains #{}]
   ;;   (if (empty? stack)
   ;;     chains
   ;;     (let [chain (peek stack)
   ;;           exts (chain-extensions chain)]
   ;;       (if (empty? exts)
   ;;         (recur (pop stack) (conj chains chain))
    ;;         (recur (into (pop stack) exts) chains)))))
    (kigen.orbit/dfs [[e]] chain-extensions)
    ))

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
             (reduce #(assoc % %2 (conj (% %2) d)) dists frontier)
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
