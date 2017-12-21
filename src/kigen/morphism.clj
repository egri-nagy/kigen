(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms, index -> image

  These functions are relatively inefficient (compared to generator table
  methods). More for reference purposes, not for the high-end computations."
  (:require [kigen.multab :as multab :refer [at]]
            [orbit.core :refer [acyclic-search-single]]
            [clojure.set :refer [subset? difference]]
            [kigen.combinatorics :refer [non-empty-subsets
                                         big-enough-partitions]]))

(declare many-to-1-morphism-search ; the backtrack search algorithm
         one-to-1-morphism-search
                                        ; high-level functions
         relmorphisms
         divisions
         morphisms
         isomorphisms
                                        ; predicates for deciding the morphic property
         relmorphic?
         homomorphic?
         homomorphic2?
         isomorphic?)

;;------------------------------------------------------------------------------
;; high-level, easy to call user functions for finding morphisms

(defn relmorphisms "All relational morphisms from S to T."
  [S T]
  (many-to-1-morphism-search S T []
                             (set (non-empty-subsets (multab/elts T)))
                             relmorphic?))

(defn homomorphisms "All homomorphisms from S to T."
  [S T]
  (many-to-1-morphism-search S T []
                             (set (multab/elts T))
                             homomorphic?))

(defn homomorphisms2
  "All homomorphisms from S to T."
  [S T]
  (let [sa (fn [hom]
             (if (= (count hom) (count S))
               #{}
               (set (filter (partial isomorphic? S T)
                            (map (partial conj hom)
                                 (multab/elts T))))))]
    (acyclic-search-single [[]] sa (fn [v] (= (count v) (count S))))))


(defn divisions
  "All divisions from S to T."
  [S T]
  (distinct
   (mapcat
    #(let [cands (set %)]
       (one-to-1-morphism-search S T []
                                 relmorphic?
                                 (fn [x] cands)))
    (big-enough-partitions (multab/elts T) (count S)))))

(defn isomorphisms
  "All isomorphisms from S to T."
  [S T]
  (let [TbyIP (group-by (partial multab/index-period T)
                        (multab/elts T))
        TsetsbyIP (into {} (map (fn [k] [k (set (TbyIP k))])
                                (keys TbyIP)))
        Sips (map (partial multab/index-period S)
                  (multab/elts S))
        cands-fn (mapv TsetsbyIP Sips)
        sa (fn [hom]
             (if (= (count hom) (count S))
               #{}
               (let [ts (cands-fn (count hom))
                     rts (remove (set hom) ts)]
                 (set (filter (partial isomorphic? S T)
                              (map (partial conj hom) rts))))))]
    (acyclic-search-single [[]] sa (fn [v] (= (count v) (count S))))))

;;------------------------------------------------------------------------------
;; the generic search algorithms

(defn one-to-1-morphism-search
  "A backtrack search for constructing lossless morphisms from a source
  multiplication table S to a target T. A partial morphism can be given to
  constrain the search (TODO the partial part is not done yet) or an empty
  vector to get all. The predicate function morphic? checks whether a new
  extension is morphic or not according to the type of the morphism."
  [S T hom morphic? cand-fn]
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
                                          (morphic? S T nhom ndom ncod))
                                        extensions)]
                (mapcat ; recursion here
                 (fn [[nhom ncod used]] (backtrack nhom ndom ncod used))
                 morphic-extensions))))]
    (backtrack hom (vec (range (count hom))) (set hom) #{})))

(defn many-to-1-morphism-search
  "A backtrack search for constructing lossy morphisms from a source multiplication
  table S to a target T. A partial morphism can be given to constrain the search
  (TODO the partial part is not done yet) or an empty vector to get all.
  The predicate function morphic? checks whether a new extension is morphic or
  not according to the type of the morphism."
  [S T hom cands morphic?]
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
                                          (morphic? S T nhom ndom ncod))
                                        extensions)]
                (mapcat ; recursion here
                 (fn [[nhom ncod]] (backtrack nhom ndom ncod))
                 morphic-extensions))))]
    (backtrack hom (vec (range (count hom))) (set hom))))

;;------------------------------------------------------------------------------
;; Predicates for checking the 'morphicity' of a mapping.
;; These rely on lazy evaluation, still they can be redundant in checks.

(defn relmorphic?
  "Decides whether the (partial) mapping hom from S to T with given domain and
  codomainis a relational morphism or not."
  [S T hom dom cod]
  (letfn [(fail?
            [[x y]]
            (let [xy (at S x y)
                  XY (multab/set-mul T (hom x) (hom y))]
              (and (contains? dom xy)
                   (not (subset? XY (hom xy))))))]
    (not-any? fail?
              (for [x dom y dom]
                [x y]))))

(defn homomorphic?
  "Decides whether the mapping hom from S to T is homomorphic or not."
  [S T hom dom cod]
  (letfn [(fail?
            [[x y]]
            (let [xy (at S x y)
                  XY (at T (hom x) (hom y))]
              (if (contains? cod XY)
                (and (contains? dom xy) (not= XY (hom xy)))
                (= (count dom) (count S)))))]
    (not-any? fail?
              (for [x dom y dom]
                [x y]))))

(defn isomorphic?
  "Decides whether the mapping hom from S to T is isomorphic or not."
  [S T hom]
  (let [dom (range (count hom))
        cod (set hom)
        totally? (fn [[x y]]
                   (= (hom (at S x y))
                      (at T (hom x) (hom y))))
        partially? (fn [[x y]]
                     (let [xy (at S x y)
                           XY (at T (hom x) (hom y))]
                       (if (contains? hom xy) ;key containment
                         (= XY (hom xy))
                         true)));(not (contains? cod XY)))))
        good? (if (= (count hom) (count S))
                totally?
                partially?)]
    (every? good?
            (for [x dom y dom]
              [x y]))))
