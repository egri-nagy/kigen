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
  Volume 7, Number 2, pages 318-335, July 2017
  https://doi.org/10.15803/ijnc.7.2_318"
  (:require
   [clojure.set :refer [subset?]]
   [orbit.core :refer [terminating-tree-search tree-search]]
   [kigen.multab :as multab]
   [kigen.combinatorics :refer [non-empty-subsets
                                big-enough-partitions]]
   [kigen.morphic :refer [morphism?]]))

(declare relmorphisms ; high-level functions
         divisions
         homomorphisms
         isomorphisms)

;;------------------------------------------------------------------------------
;; Predicates for checking the morphic-property of mappings.

(defn multab-relmorphism?
  "Decides whether the (partial) mapping morph-m from multab S to multab T is
   a relational morphism or not. "
  [S T morph-m] 
  (morphism?
   subset?
   (fn [a b] ((S a) b)) ;;multiplication in the table
   (partial multab/set-mul T)
   morph-m))

(defn multab-homomorphism?
  "Decides whether the partial mapping morph-m from S to T is homomorphic or not.
  It lazily checks all products."
  [S T morph-m] 
  (morphism?
   =
   (fn [a b] ((S a) b)) ;;multiplication in the table
   (fn [a b] ((T a) b))
   morph-m))

;;------------------------------------------------------------------------------
;; high-level functions for finding all morphisms of a given type
;; only the soruce and target multiplication tables needs to be given

;; the generic generator function for extending morphisms, to use with
;; orbit's tree-search
(defn generator-fn
  "For extension mechanisms for fixed candidate sets.
   We extend the morphism by a new mapping using the size of the hash-map as
   the key for the new entry, using candidates for possible values."
  [S morphims?-fn candidates]
  (fn [morph-m]
    (if (= (count S) (count morph-m)) ;stop overextending
      #{}
      (filter morphims?-fn
              (map (partial conj morph-m)
                   (map (fn [A] [(count morph-m) A]);the next number is the key 
                        candidates))))))

(defn relmorphisms
  "All relational morphisms from multiplication table `S` to `T`.
  These are one-to-many set-valued morphic mappings."
  [S T] 
  (tree-search [{}]
               (generator-fn S
                             (partial multab-relmorphism? S T)
                             (non-empty-subsets (multab/elts T)))
               (fn [morph-m] ;we have a solution when the mapping is total
                 (= (count S) (count morph-m)))))

(defn homomorphisms
  "All homomorphisms from multiplication table `S` to `T`."
  [S T] 
  (tree-search [{}]
               (generator-fn S
                             (partial multab-homomorphism? S T)
                             (multab/elts T))
               (fn [morph-m]
                 (= (count S) (count morph-m)))))

;; these below are more complicated as they have changing candidates

;; injective relational morphism - the image sets do not overlap
(defn divisions
  "All divisions from S to T."
  [S T]
  (distinct
   (mapcat
    (fn [partition]
      (letfn [(sol?
               [morph-m]
               (= (count S) (count morph-m)))
              (generator-fn
               [morph-m]
               (let [mappings (map (fn [a] [(count morph-m) a])
                                   (remove (set (vals morph-m)) partition))]
                 (filter (partial multab-relmorphism? S T)
                         (map (partial conj morph-m) mappings))))]
        (terminating-tree-search [{}] generator-fn sol?)))
    ;;we need a distinct subset for each s in S
    (big-enough-partitions (multab/elts T)
                           (count S)))))

(defn isomorphisms
  "All isomorphisms from S to T."
  [S T]
  (let [;;elements of T classified by their index-period value pairs
        TbyIP (group-by (partial multab/index-period T)
                        (multab/elts T))
        ;; lookup table for elements of with a given index-period
        TsetsbyIP (into {} (map (fn [k] [k (set (TbyIP k))])
                                (keys TbyIP)))
        ;; index-periods for elements of S   TODO: ordering is assumed here!
        SIPs (map (partial multab/index-period S)
                  (multab/elts S))
        ;; gives for an element of S the set of possible targets
        cands-fn (mapv TsetsbyIP SIPs)
        generator-fn (fn [morph-m]
                       (let [ts (cands-fn (count morph-m))
                             rts (remove (set morph-m) ts)]
                         (filter (partial multab-homomorphism? S T)
                                 (map (partial conj morph-m)
                                      (map (fn [a] [(count morph-m) a]) rts)))))
        sol? (fn [morph-m] (= (count S) (count morph-m)))]
    (terminating-tree-search [{}]
                             generator-fn
                             sol?)))