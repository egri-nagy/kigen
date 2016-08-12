;; partially ordered sets
;; ways of defining relations:
;; 1. elements and a relation function (implicit)
;; 2. a map: elements -> set of related elements (explicit)
(ns kigen.poset
  (:use [clojure.set :only [union]]))

(declare rel ;explicit relation
         cover-rel ;calculates the covering relation of a relation
         chain-extensions ;extends chain by one more element
         all-chains ;all chains from an element
         max-distances) ;maximal distances from an element

(defn rel
  "Makes an implicit relation explicit. Works for finite set of elements."
  [elts rel?]
  (into {} (map #(vec [% (set (filter (fn [x] (rel? x %)) elts))]) elts)))

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
                        (if (some #(rel? newval %) covers)
                          covers ;newval is below some of covers, no change
                          (conj
                           (set (filter #(not (rel? % newval)) covers))
                           newval))) ;the subset of covers not below and newval
        insert (fn [cr e] ;insert an element into the graph by updating covers
                 (let [xs (filter #(and (not (= e %)) (rel? e %)) elts)]
                   (reduce #(assoc % %2 (recalc-covers (% %2) e)) cr xs)))]
    (reduce insert emptytab elts)))

(defn chain-extensions
  "Returns all chains extending chain by one more element in cover-rel cr."
  [chain cr]
  (map (partial conj chain) (cr (last chain))))

(defn all-chains
  "All chains from element e in the cover-rel cr."
  [e cr]
  (loop [stack [[e]] chains #{}]
    (if (empty? stack)
      chains
      (let [chain (peek stack)
            exts (chain-extensions chain cr)]
        (if (empty? exts)
          (recur (pop stack) (conj chains chain))
          (recur (into (pop stack) exts) chains))))))

(defn max-distances
  "Returns a map of elements to their maximal distance (length of longest chain)
  from element e."
  [e cr]
  (loop [d 0
         dists {}
         frontier #{e}]
    (if (empty? frontier)
      dists
      (recur (inc d)
             (reduce #(assoc % %2 d) dists frontier)
             (apply union (map cr frontier))))))

(defn min-distances
  "Returns a map of elements to their minimal distance
   (length of shortest chain) from element e."
  [e cr]
  (loop [d 0
         dists {}
         frontier #{e}]
    (if (empty? frontier)
      dists
      (recur (inc d)
             (reduce #(update % %2 (fn [x] (if (nil? x) d x))) dists frontier)
             (apply union (map cr frontier))))))
