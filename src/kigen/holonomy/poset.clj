(ns kigen.holonomy.poset
  "Computing partially ordered sets. Ways of defining binary relations:
  1. elements and a relation function (implicit)
  2. a map: element x -> set of related elements (explicit)")

(declare rel ;explicit relation
         cover-rel) ;calculates the covering relation of a relation

(defn rel
  "Makes an implicit relation explicit. Works for finite set of elements."
  [elts rel?]
  (zipmap elts
          (map (fn [x] (set (filter (partial  rel? x)
                                    elts)))
               elts)))

(defn inverse
  "Given an implicit relation by function rel? this returns the function
  for the inverse relation."
  [rel?]
  (fn [x y] (rel? y x))) ; just swapping arguments

;; A cubic algorithm for finding covers in a binary relation.
;; It assumes that anything related is a cover
;; then gets rid of an element if it is proven not to be a cover.
;; elts - set of elements
;; rel? - a partial order relation predicate, (rel? a b) Is a below b?
(defn cover-rel
  "The covering relation of a partial order as a graph
  (map: element -> set of covers), given by the elements and a predicate."
  [elts rel?]
  (let [emptytab (into {} (for [e elts] [e #{}])) ;empty relation
        recalc-covers (fn [covers newval] ;recalculate the set of covers
                        (if (some #(rel? % newval) covers)
                          covers ;newval is below some of covers, no change
                          (conj
                           (set (remove (partial rel? newval) covers))
                           newval))) ;the subset of covers not below and newval
        insert (fn [tab e] ;insert an element into the graph by updating covers
                 ; finding all xs related to e, x rel e, but not equal to e
                 (let [related (filter
                                #(and (not= % e) (rel? % e))
                                elts)]
                   ; updating table entries for all related
                   (reduce #(update-in %1 [%2] recalc-covers e)
                           tab
                           related)))]
    (reduce insert emptytab elts)))
