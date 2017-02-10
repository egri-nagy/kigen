(ns kigen.transf
  "Transformations and permutations. Several embeddings into partitioned
  binary relations. Also, simple representation as a vector."
  (:require [kigen.pbr :as pbr]))

(declare singleton?
         transf->binrel
         binrel->transf
         binrel-transf?
         transf-binrel-degree
         transf->bipart
         bipart->transf)

(defn singleton? [coll] (= 1 (count coll)))

;; BINARY RELATION EMBEDDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this is embedding into the binary relation subsemigroup
;;not the partition monoid
(defn transf->binrel
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

(defn binrel->transf
  "Returns the image list of a transformation represented as a pbr.
  Indexing is 1-based."
  [pbr]
  (let [n (count (:dom pbr))]
    (map #(- (first (pbr %)) n) (-> (:dom pbr) seq sort))))

(defn binrel-transf?
  [pbr]
  (and (empty? (reduce into (for [key (:cod pbr)] (pbr key))))
       (every?  singleton? (for [key (:dom pbr)] (pbr key)))))

(defn transf-binrel-degree [pbr] (count (:dom pbr)))

;;DEFAULT representation of transformations as pbr
(def ->transf binrel->transf)
(def transf-> transf->binrel)
(def transf-degree transf-binrel-degree)

;; BIPARTITION EMBEDDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transf->bipart
  "Creates a partitioned binary relation with undirected edges
  from a transformation given by the list of images.
  Embedding into the (bi)partition monoid.
  Transformations index point from 1, unlike the vector indices."
  [imagelist]
  (let [t (transf->binrel imagelist)]
    (pbr/overlay t (pbr/rev t))))

(defn bipart->transf
  [pbr]
  (vec (map #(- % (count (:dom pbr)))
            (mapcat (fn [x] (filter (:cod pbr) (pbr x)) ) (sort (:dom pbr))))))

;; STANDARD GENERATING SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn idmap [n] (vec (range n)))
(defn transposition [n] (vec (concat [1 0] (range 2 n))))
(defn ncycle [n] (vec (concat (range 1 n) [0])))
(defn collapsing [n] (vec (concat [0 0] (range 2 n))))

(defn symmetric-gens
  "Generators of the symmetric group of degree n using the embedding
  into the partitioned binary relation monoid defined by f."
  [n]
  (if (= 1 n)
    [[0]]
    [(transposition n) (ncycle n)]))

(defn full-ts-gens
  "Generators of the full transformation semigroup of degree n."
  [n]
  (if (= 1 n)
    (symmetric-gens n)
    (concat (symmetric-gens n) [(collapsing n)])))

(defn mul
  "Right multiplication of transformations represented by vectors."
  [s t]
  (mapv t s)) ; as simple as that

;;acting as pbr, then shift back the resulting set to have a transformation of
;;the canonical set 1..n ; TODO does this work for both representations?
(defn act
  [points t]
  (map t points))
