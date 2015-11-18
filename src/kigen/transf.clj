(ns kigen.transf
  (:require [kigen.pbr :as pbr]))

(defn transf-degree [pbr] (count (:dom pbr)))

(defn singleton? [coll] (= 1 (count coll)))

(defn transf?
  [pbr]
  (and (empty? (reduce into (for [key (:cod pbr)] (pbr key))))
       (every?  singleton? (for [key (:dom pbr)] (pbr key)))))

;;this is embedding into the binary relation subsemigroup
;;not the partition monoid
(defn transf->pbr
  "Creates a partitioned binary relation with directed edges
  from a transformation given by the list of images.
  Transformations index point from 1, unlike the vector indices."
  [imagelist]
  (let [emptyset #{}
        n (count imagelist)
        pbr {:dom (set (range 1 (inc n)))
             :cod (set (range (inc n) (inc (* 2 n))))}
        edges (into {} (map
                        #(vector % (set [(+ n (nth imagelist (dec %)))]))
                        (:dom pbr)))
        non-edges (into {} (map
                            #(vector % emptyset)
                            (:cod pbr)))]
    (reduce into pbr (concat [edges non-edges]))))

(defn transf->pbr2
  "Creates a partitioned binary relation with undirected edges
  from a transformation given by the list of images.
  Embedding into the partition monoid.
  Transformations index point from 1, unlike the vector indices."
  [imagelist]
  (let [t (transf->pbr imagelist)]
    (pbr/overlay t (pbr/rev t))))

(defn pbr->transf
  "Returns the image list of a transformation represented as a pbr.
  Indexing is 1-based."
  [pbr]
  (let [n (count (:dom pbr))]
    (map #(- (first (pbr %)) n) (-> (:dom pbr) seq sort))))

(defn transf-compare
  [x y]
  (compare (vec (pbr->transf x)) (vec (pbr->transf y))))

(defn symmetric-gens
  "Generators of the symmetric group of degree n."
  ([n] (symmetric-gens n transf->pbr))
  ([n f] (if (= 1 n)
           (map f [[1]])
           (let [transposition (concat [2 1] (range 3 (inc n)))
                 cycle (concat (range 2 (inc n)) [1])]
             (map f (set [transposition cycle]))))))

(defn full-ts-gens
  "Generators of the full transformation semigroup of degree n."
  [n]
  (if (= 1 n)
    (symmetric-gens n)
    (let [collapse (concat [1 1] (range 3 (inc n)))]
      (concat (symmetric-gens n) [(transf->pbr collapse)]))))

;;acting as pbr, then shift back the resulting set to have a transformation of
;;the canonical set 1..n
(defn act
  [points t]
  (set (map #(- % (count (:dom t))) (pbr/act points t))))
