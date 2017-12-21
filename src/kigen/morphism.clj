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

(declare one-to-1-morphism-search
                                        ; high-level functions
         relmorphisms
         divisions
         homomorphisms
         isomorphisms
                                        ; predicates for deciding the morphic property
         relmorphic?
         homomorphic?)

;;------------------------------------------------------------------------------
;; high-level, easy to call user functions for finding morphisms

(defn relmorphisms
  "All relational morphisms from S to T."
  [S T]
  (let [sa (fn [hom]
             (if (= (count hom) (count S))
               #{}
               (set (filter (partial relmorphic? S T)
                            (map (partial conj hom)
                                 (set (non-empty-subsets (multab/elts T))))))))]
    (acyclic-search-single [[]] sa (fn [v] (= (count v) (count S))))))


(defn homomorphisms
  "All homomorphisms from S to T."
  [S T]
  (let [sa (fn [hom]
             (if (= (count hom) (count S))
               #{}
               (set (filter (partial homomorphic? S T)
                            (map (partial conj hom)
                                 (multab/elts T))))))]
    (acyclic-search-single [[]] sa (fn [v] (= (count v) (count S))))))

(defn divisions
  "All divisions from S to T."
  [S T]
  (distinct
   (mapcat
    (fn [partition]
      (let [sa (fn [hom]
             (if (= (count hom) (count S))
               #{}
               (let [rts (remove (set hom) partition)]
                 (set (filter (partial relmorphic? S T)
                              (map (partial conj hom) rts))))))]
        (acyclic-search-single [[]] sa (fn [v] (= (count v) (count S))))))
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
                 (set (filter (partial homomorphic? S T)
                              (map (partial conj hom) rts))))))]
    (acyclic-search-single [[]] sa (fn [v] (= (count v) (count S))))))

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
  "Decides whether the mapping hom from S to T is isomorphic or not."
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
