(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms, index i -> image

  These functions are relatively inefficient (compared to generator table
  methods). More for reference purposes, not for the high-end computations.

  This is a reference implementation for the paper:
  Finite Computational Structures and Implementations: Semigroups and
  Morphic Relations
  International Journal of Networking and Computing,
  Volume 7, Number 2, pages 318–335, July 2017"
  (:require [kigen.multab :as multab :refer [at]]
            [orbit.core :refer [tree-search]]
            [clojure.set :refer [subset? difference]]
            [kigen.combinatorics :refer [non-empty-subsets
                                         big-enough-partitions]]))

(declare relmorphisms ; high-level functions
         divisions
         homomorphisms
         isomorphisms
         relmorphic? ; predicates for deciding the morphic property
         homomorphic?)

;;------------------------------------------------------------------------------
;; high-level functions for finding all morphisms of a given type
;; only the soruce and target multiplication tables needs to be given

(defn total?
  "Returns true if the given morphism is total, false if it is partial."
  [S hom]
  (= (count S) (count hom)))

(defn relmorphisms
  "All relational morphisms from S to T. These are one-to-many set-valued
  morphic mappings."
  [S T]
  (letfn [(generator [hom]
            (if (total? S hom)
              #{}
              (filter (partial relmorphic? S T)
                      (map (partial conj hom)
                           (non-empty-subsets (multab/elts T))))))]
    (tree-search [[]]
                 generator
                 (fn [v] (total? S v)))))

(defn homomorphisms
  "All homomorphisms from S to T."
  [S T]
  (letfn [(generator [hom]
            (if (total? S hom)
              #{}
              (filter (partial homomorphic? S T)
                      (map (partial conj hom)
                           (multab/elts T)))))]
    (tree-search [[]]
                 generator
                 (fn [v] (total? S v)))))

(defn divisions
  "All divisions from S to T."
  [S T]
  (distinct
   (mapcat
    (fn [partition]
      (letfn [(generator [hom]
                (if (total? S hom)
                  #{}
                  (let [rts (remove (set hom) partition)]
                    (filter (partial relmorphic? S T)
                            (map (partial conj hom) rts)))))]
        (tree-search [[]]
                     generator
                     (fn [v] (total? S v)))))
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
        generator (fn [hom]
             (if (total? S hom)
               #{}
               (let [ts (cands-fn (count hom))
                     rts (remove (set hom) ts)]
                 (filter (partial homomorphic? S T)
                         (map (partial conj hom) rts)))))]
    (tree-search [[]]
                 generator
                 (fn [v] (total? S v)))))

;;------------------------------------------------------------------------------
;; Predicates for checking the 'morphicity' of a mapping.
;; These rely on lazy evaluation, still they can be redundant in checks.

(defn relmorphic?
  "Decides whether the (partial) mapping hom from S to T with given domain and
  codomain is a relational morphism or not."
  [S T hom]
  (let [k (count hom)
        dom (range k)
        fail? (fn
            [[x y]]
            (let [xy (at S x y)
                  XY (multab/set-mul T (hom x) (hom y))]
              (and (< xy k)
                   (not (subset? XY (hom xy))))))]
    (not-any? fail?
              (for [x dom y dom]
                [x y]))))


(defn homomorphic?
  "Decides whether the partial mapping hom from S to T is homomorphic or not.
  It lazily checks all products."
  [S T hom]
  (let [k (count hom)
        dom (range k)
        good? (fn [[x y]]
                (let [xy (at S x y)]
                  (if (< xy k)
                    (= (hom xy)
                       (at T (hom x) (hom y)))
                    true)))]
    (every? good?
            (for [x dom y dom]
              [x y]))))
