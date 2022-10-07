(ns kigen.transf-pbr
  "Transformations and permutations embedded into partitioned
  binary relations."
  (:require [kigen.pbr :as pbr]
            [kigen.combinatorics :refer [singleton?]]))

(declare transf->binrel
         binrel->transf
         binrel-transf?
         transf-binrel-degree
         transf->bipart
         bipart->transf)

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
