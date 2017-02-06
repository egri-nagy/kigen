(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms"
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset?]]))

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

(defn morphisms
  "S source multab
   T target multab
  hom - (partial) morphism
  morphic? - predicate deciding whether a map is a (potential) morphism or not
  targets - subset of T that are possible targets of the morphism"
  ([S,T,hom, morphic?] (morphisms S T hom morphic? (multab/elts T)))
  ([S,T,hom, morphic?, targets]
   (letfn [(backtrack [hom dom cod]
             (if (= (count hom) (count S))
               [hom]
               (let [ndom (conj dom (count hom))
                     extensions (map
                                 (fn [x] [(conj hom x) (conj cod x)])
                                 targets)
                     morphic-extensions (filter
                                         (fn [[nhom ncod]]
                                           (morphic? S T nhom ndom ncod))
                                         extensions)]
                 (mapcat
                  (fn [[nhom ncod]] (backtrack nhom ndom ncod))
                  morphic-extensions))))]
     (backtrack hom (vec (range (count hom))) (set hom)))))

(defn isomorphic?
  "Decides whether the mapping hom from S to T is homomorphic or not."
  [S T hom]
  (let [dom (vec (range (count hom)))
        cod (set hom)]
    (letfn [(good? [x y] (let [z (at S x y)
                           XY (at T (hom x) (hom y))]
                       (if  (contains? dom z)
                         (= XY (hom z))
                         (not (contains? cod XY)))))]
      (nil? (first (for [x dom y dom :when (not (good? x y))] [x y]))))))

(defn isomorphisms
  "S source multab
   T target multab
  hom - (partial) morphism
  morphic? - predicate deciding whether a map is a (potential) morphism or not
  choices - the possible next elements based on partial solution and source"
  [S,T,hom,  morphic?]
  (let [Tset (multab/elts T)]
    (letfn [(backtrack  [hom choices]
              (if (= (count hom) (count S))
                [hom]
                (let [valid-choices (remove (set hom) choices)
                      extensions (filter
                                  #(morphic? S T %)
                                  (map #(conj hom %) valid-choices))]
                  (mapcat #(backtrack % choices)  extensions))))]
      (backtrack hom Tset))))
