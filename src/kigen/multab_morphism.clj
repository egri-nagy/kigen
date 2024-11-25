(ns kigen.multab-morphism
  "Constructing morphisms and morphic relations for multiplication tables.
  input: two multiplication tables (source, target)
  output: hash-maps describing morphisms, index i -> image

  These functions are relatively inefficient (compared to generator table
  methods).
  They are for reference purposes, not for the high-end computations.

  This is a reference implementation for the paper:
  Finite Computational Structures and Implementations: Semigroups and
  Morphic Relations
  International Journal of Networking and Computing,
  Volume 7, Number 2, pages 318–335, July 2017
  https://doi.org/10.15803/ijnc.7.2_318"
  (:require
   [clojure.set :refer [subset?]]
   [orbit.core :refer [tree-search]]
   [kigen.multab :as multab]
   [kigen.combinatorics :refer [non-empty-subsets
                                big-enough-partitions]]
   [kigen.morphic :refer [morphism?]]))

(declare relmorphisms ; high-level functions
         divisions
         homomorphisms
         isomorphisms)

;;------------------------------------------------------------------------------
;; Predicates for checking the 'morphicity' of a mapping.
;; These rely on lazy evaluation, still they can be redundant in checks.

(defn multab-relmorphism?
  "Decides whether the (partial) mapping morph-m from multab S to multab T is
   a relational morphism or not. "
  [S T morph-m] 
  (morphism?
   subset?
   (fn [a b] ((S a) b)) ;;replacing the macro
   (partial multab/set-mul T)
   morph-m))

(defn multab-homomorphism?
  "Decides whether the partial mapping morph-m from S to T is homomorphic or not.
  It lazily checks all products."
  [S T morph-m] 
  (morphism?
   =
   (fn [a b] ((S a) b)) ;;replacing the macro
   (fn [a b] ((T a) b))
   morph-m))

;;------------------------------------------------------------------------------
;; high-level functions for finding all morphisms of a given type
;; only the soruce and target multiplication tables needs to be given

(defn total?
  "Returns true if the given morphism is total, false if it is partial."
  [S morph-m]
  (= (count S) (count morph-m)))

(defn relmorphisms
  "All relational morphisms from S to T. These are one-to-many set-valued
  morphic mappings."
  [S T]
  (letfn [(generator [morph-m]
            (if (total? S morph-m)
              #{}
              (filter (partial multab-relmorphism? S T)
                      (map (partial conj morph-m)
                           (map (fn [A] [(count morph-m) A])
                            (non-empty-subsets (multab/elts T)))))))]
    (tree-search [{}]
                 generator
                 (fn [v] (total? S v)))))

(defn homomorphisms
  "All homomorphisms from S to T."
  [S T]
  (letfn [(generator [morph-m]
            (if (total? S morph-m)
              #{}
              (filter (partial multab-homomorphism? S T)
                      (map (partial conj morph-m)
                           (map (fn [a] [(count morph-m) a])
                                (multab/elts T))))))]
    (tree-search [{}]
                 generator
                 (fn [v] (total? S v)))))

(defn divisions
  "All divisions from S to T."
  [S T]
  (distinct
   (mapcat
    (fn [partition]
      (letfn [(generator
                [morph-m]
                (if (total? S morph-m)
                  #{}
                  (let [rts (remove (set (vals morph-m)) partition)] ;here we use the fact that morph-m is a hash-map, maybe ok
                    (filter (partial multab-relmorphism? S T)
                            (map (partial conj morph-m)
                                 (map (fn [a] [(count morph-m) a]) rts))))))]
        (tree-search [{}]
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
        generator (fn [morph-m]
             (if (total? S morph-m)
               #{}
               (let [ts (cands-fn (count morph-m))
                     rts (remove (set morph-m) ts)]
                 (filter (partial multab-homomorphism? S T)
                         (map (partial conj morph-m) 
                              (map (fn [a] [(count morph-m) a]) rts))))))]
    (tree-search [{}]
                 generator
                 (fn [v] (total? S v)))))