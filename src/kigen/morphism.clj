(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms, index -> image"
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset?]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare morphism-search ; the backtrack search algorithm
         ; high-level functions
         relmorphisms
         divisions
         morphisms
         isomorphisms
         ; predicates for deciding the morphic property
         relmorphic?
         morphic?
         isomorphic?
         ; update functions for the set of candidates
         one-to-one
         one-to-many
         ; util functions for setting up set-valued candidates
         non-empty-subsets
         big-enough-partitions
         )

;;------------------------------------------------------------------------------
;; high-level, easy to call user functions for finding morphisms

(defn relmorphisms "All relational morphisms from S to T."
  [S T]
  (morphism-search S T []
                   (set (non-empty-subsets (multab/elts T)))
                   relmorphic?
                   one-to-many))

(defn divisions
  "All divisions from S to T."
  [S T]
  (mapcat
   #(morphism-search S T []
                     (set %)
                     relmorphic?
                     one-to-one)
   (big-enough-partitions S (multab/elts T))))

(defn homomorphisms "All homomorphisms from S to T."
  [S T]
  (morphism-search S T [] (set (multab/elts T)) morphic? one-to-many))

(defn isomorphisms
  "All isomorphisms from S to T."
  [S T]
  (morphism-search S T [] (set (multab/elts T)) isomorphic? one-to-one))

;;------------------------------------------------------------------------------
;; the generic search algorithm

(defn morphism-search
  "A backtrack search for constructing morphisms from a source multiplication
  table S to a target T. A partial morphism can be given to constrain the search
  (TODO the partial part is not done yet) or an empty vector to get all.
  The predicate function accept? checks whether a new extension is morphic or
  not according to the type of the morphism. The function cands-update-fn keeps
  track of the possible candidates at each extension."
  [S,T,hom, cands, accept?, cands-update-fn]
  (letfn [(backtrack [hom dom cod cands]
            (if (= (count hom) (count S))
              [hom]
              (let [ndom (conj dom (count hom))
                    extensions (map ; creating argument vector for recursion
                                (fn [x] [(conj hom x)
                                         (conj cod x)
                                         (cands-update-fn cands x)])
                                cands)
                    morphic-extensions (filter
                                        (fn [[nhom ncod]]
                                          (accept? S T nhom ndom ncod))
                                        extensions)]
                (mapcat ; recursion here
                 (fn [[nhom ncod tgs]] (backtrack nhom ndom ncod tgs))
                 morphic-extensions))))]
    (backtrack hom (vec (range (count hom))) (set hom) cands)))

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
;; functions for updating the set of candidates in backtrack when extending by x

(defn one-to-many
  "Candidate set update function. General morphisms allow the same image for
  different elements, so candidate set just returned."
  [cands x] cands)

(defn one-to-one
  "Candidate set update function. Isomorphisms/divisions need to have 1-to-1
  maps. When extending with x, we need to disjoin x from candidates."
  [targets x] (disj  targets x))

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
