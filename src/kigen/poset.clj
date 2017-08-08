(ns kigen.poset
  "Computing partially ordered sets. Ways of defining binary relations:
  1. elements and a relation function (implicit)
  2. a map: element x -> set of related elements (explicit)")

(declare rel ;explicit relation
         cover-rel ;calculates the covering relation of a relation
         chains ;all chains between given elements
         max-distances) ;maximal distances from an element

(defn rel
  "Makes an implicit relation explicit. Works for finite set of elements."
  [elts rel?]
  (into {} (map
            (fn [x] [x (set (filter (partial  rel? x) elts))])
            elts)))

(defn inverse
  "Given an implicit relation by function rel? this returns the function
  for the inverse relation."
  [rel?]
  (fn [x y] (rel? y x))) ; just swapping arguments

(defn equivalent
  "Given a preorder relation, it returns the corresponding partial order
  implicit relation."
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
                 (let [xs (filter #(and (not= e %) (rel? % e)) elts)]
                   (reduce #(update-in % [%2] recalc-covers e) cr xs)))]
    (reduce insert emptytab elts)))
