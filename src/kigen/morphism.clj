(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms, index -> image"
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset? difference]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare many-to-1-morphism-search ; the backtrack search algorithm
         one-to-1-morphism-search
         ; high-level functions
         relmorphisms
         divisions
         morphisms
         isomorphisms
         ; predicates for deciding the morphic property
         relmorphic?
         morphic?
         isomorphic?
         ; util functions for setting up set-valued candidates
         non-empty-subsets
         big-enough-partitions
         )

;;------------------------------------------------------------------------------
;; high-level, easy to call user functions for finding morphisms

(defn relmorphisms "All relational morphisms from S to T."
  [S T]
  (many-to-1-morphism-search S T []
                             (set (non-empty-subsets (multab/elts T)))
                             relmorphic?))

(defn homomorphisms "All homomorphisms from S to T."
  [S T]
  (many-to-1-morphism-search S T [] (set (multab/elts T)) morphic?))

(defn divisions
  "All divisions from S to T."
  [S T]
  (mapcat
   #(let [cands (set %)]
      (one-to-1-morphism-search S T []
                                cands
                                relmorphic?
                                (fn [x] cands)))
   (big-enough-partitions S (multab/elts T))))

(defn isomorphisms
  "All isomorphisms from S to T."
  [S T]
  (let [cands (set (multab/elts T))
        m (group-by #(multab/index-period T %) cands)
        ippairs->Tsubsets (reduce #(update-in %1 [%2] set) m (keys m))
        Sips (map #(multab/index-period S %)  (range (count S)))
        cands-fn (mapv ippairs->Tsubsets Sips)]
    (one-to-1-morphism-search S T [] cands isomorphic? cands-fn)))


;;------------------------------------------------------------------------------
;; the generic search algorithms

(defn one-to-1-morphism-search
  "A backtrack search for constructing lossless morphisms from a source
  multiplicationctable S to a target T. A partial morphism can be given to
  constrain the search (TODO the partial part is not done yet) or an empty
  vector to get all. The predicate function accept? checks whether a new
  extension is morphic or not according to the type of the morphism."
  [S,T,hom, cands, accept? cand-fn]
  (letfn [(backtrack [hom dom cod used]
            (if (= (count hom) (count S))
              [hom]
              (let [ndom (conj dom (count hom))
                    extensions (map ; creating argument vector for recursion
                                (fn [x] [(conj hom x)
                                         (conj cod x)
                                         (conj used x)])
                                (difference (cand-fn (peek ndom)) used))
                    morphic-extensions (filter
                                        (fn [[nhom ncod]]
                                          (accept? S T nhom ndom ncod))
                                        extensions)]
                (mapcat ; recursion here
                 (fn [[nhom ncod used]] (backtrack nhom ndom ncod used))
                 morphic-extensions))))]
    (backtrack hom (vec (range (count hom))) (set hom) #{})))

(defn many-to-1-morphism-search
  "A backtrack search for constructing lossy morphisms from a source multiplication
  table S to a target T. A partial morphism can be given to constrain the search
  (TODO the partial part is not done yet) or an empty vector to get all.
  The predicate function accept? checks whether a new extension is morphic or
  not according to the type of the morphism."
  [S,T,hom, cands, accept?]
  (letfn [(backtrack [hom dom cod]
            (if (= (count hom) (count S))
              [hom]
              (let [ndom (conj dom (count hom))
                    extensions (map ; creating argument vector for recursion
                                (fn [x] [(conj hom x)
                                         (conj cod x)])
                                cands)
                    morphic-extensions (filter
                                        (fn [[nhom ncod]]
                                          (accept? S T nhom ndom ncod))
                                        extensions)]
                (mapcat ; recursion here
                 (fn [[nhom ncod]] (backtrack nhom ndom ncod))
                 morphic-extensions))))]
    (backtrack hom (vec (range (count hom))) (set hom))))

;;------------------------------------------------------------------------------
;; accepting predicates for backtrack, checking the 'morphicity' of the map

(defn morphic?
  "Decides whether the mapping hom from S to T is homomorphic or not."
  [S T hom dom cod]
  (letfn [(fail? [x y] (let [z (at S x y)
                         XY (at T (hom x) (hom y))]
                     (if (contains? cod XY)
                       (and (contains? dom z) (not= XY (hom z)))
                       (= (count dom) (count S)))))]
      (nil? (first (for [x dom y dom :when (fail? x y)] [x y])))))


(defn relmorphic?
  "Decides whether the (partial) mapping hom from S to T with given domain and
  codomainis a relational morphism or not."
  [S T hom dom cod]
  (letfn [(fail? [x y] (let [xy (at S x y)
                             XY (multab/set-mul T (hom x) (hom y))]
                         (and (contains? dom xy)
                              (not (subset? XY (hom xy))))))]
    (nil? (first (for [x dom y dom :when (fail? x y)] [x y])))))

(defn isomorphic?
  "Decides whether the mapping hom from S to T is homomorphic or not."
  [S T hom dom cod]
  (letfn [(good? [x y] (let [z (at S x y)
                             XY (at T (hom x) (hom y))]
                         (if  (contains? dom z)
                           (= XY (hom z))
                           (not (contains? cod XY)))))]
    (nil? (first (for [x dom y dom :when (not (good? x y))] [x y])))))

;;------------------------------------------------------------------------------
;; Constructing set valued candidates for relational morphisms.

(defn non-empty-subsets
  "All subsets of T as sets, except the empty set."
  [T]
  (remove #{#{}}
          (map set (subsets (seq T)))))

(defn big-enough-partitions
  "All partitions of T with at least |S| many elements. The elements are sets."
  [S T]
  (map #(map set %)
       (filter #(<= (count S) (count %))
               (partitions (seq T)))))
