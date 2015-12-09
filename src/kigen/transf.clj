(ns kigen.transf
  (:require [kigen.pbr :as pbr]))

(defn transf-degree [pbr] (count (:dom pbr)))

(defn singleton? [coll] (= 1 (count coll)))

(defn binrel-transf?
  [pbr]
  (and (empty? (reduce into (for [key (:cod pbr)] (pbr key))))
       (every?  singleton? (for [key (:dom pbr)] (pbr key)))))

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
            (apply concat (map (fn [x] (filter (:cod pbr) (pbr x)) ) (sort (:dom pbr)))))))

(defn idmap
  [n]
  (transf->bipart (vec (range 1 (inc n)))))

(defn transf-compare
  [x y]
  (compare (vec (binrel->transf x)) (vec (binrel->transf y))))

(defn symmetric-gens
  "Generators of the symmetric group of degree n using the embedding
  into the partitioned binary relation monoid defined by f."
  ([n] (symmetric-gens n transf->bipart))
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
      (concat (symmetric-gens n) [(transf->bipart collapse)]))))

(defn symmetric-inverse-gens
  "Generators of the partial transformation monoid of degree n."
  [n]
  (let [gens (symmetric-gens n)
        id (idmap n)
        f (fn [x] #{})
        t (update id 1 f)
        tt (update t (inc n) f) ;TODO sg better than this
        ]
    (conj gens tt)))

(defn partial-ts-gens
  "Generators of the partial transformation monoid of degree n."
  [n]
  (let [gens (full-ts-gens n)
        id (transf->bipart (vec (range 1 (inc n))))
        f (fn [x] #{})
        t (update id 1 f)
        tt (update t (inc n) f) ;TODO sg better than this
        ]
    (conj gens tt))) ;TODO get rid of this copy-paste duplication

;;acting as pbr, then shift back the resulting set to have a transformation of
;;the canonical set 1..n
(defn act
  [points t]
  (set (map #(- % (count (:dom t))) (pbr/act points t))))
