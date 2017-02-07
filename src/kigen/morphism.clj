(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms"
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset?]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare morphsims)

(defn morphic?
  "Decides whether the mapping hom from S to T is homomorphic or not."
  [S T hom dom cod]
  (letfn [(fail? [x y] (let [z (at S x y)
                         XY (at T (hom x) (hom y))]
                     (if (contains? cod XY)
                       (and (contains? dom z) (not= XY (hom z)))
                       (= (count dom) (count S)))))]
      (nil? (first (for [x dom y dom :when (fail? x y)] [x y])))))

(defn non-empty-subsets [T]
  (remove #{#{}}
          (map set (subsets (seq T)))))

(defn big-enough-partitions [S T]
  (filter #(<= (count S) (count %))
          (map #(map set %) (partitions (seq T)))))

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

(defn one-to-many [targets x] targets)
(defn one-to-one [targets x] (disj  targets x))

(defn relmorphisms [S T hom]
   (morphisms one-to-many S T hom relmorphic?
             (set (non-empty-subsets (multab/elts T)))))

(defn isomorphisms [S T hom]
  (morphisms one-to-one S T hom isomorphic?))


(defn divisions [S T hom]
  (mapcat
   #(morphisms one-to-one S T hom relmorphic? (set %))
   (big-enough-partitions S (multab/elts T))))

(defn morphisms
  "S source multab
   T target multab
  hom - (partial) morphism
  morphic? - predicate deciding whether a map is a (potential) morphism or not
  targets - subset of T that are possible targets of the morphism"
  ([f, S,T,hom, morphic?] (morphisms f S T hom morphic? (multab/elts T)))
  ([f, S,T,hom, morphic?, targets]
   (letfn [(backtrack [hom dom cod targets]
             (if (= (count hom) (count S))
               [hom]
               (let [ndom (conj dom (count hom))
                     extensions (map
                                 (fn [x] [(conj hom x) (conj cod x) (f targets x)])
                                 targets)
                     morphic-extensions (filter
                                         (fn [[nhom ncod]]
                                           (morphic? S T nhom ndom ncod))
                                         extensions)]
                 (mapcat
                  (fn [[nhom ncod tgs]] (backtrack nhom ndom ncod tgs))
                  morphic-extensions))))]
     (backtrack hom (vec (range (count hom))) (set hom) targets))))
