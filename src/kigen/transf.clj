(ns kigen.transf
  "Transformations and permutations. Several embeddings into partitioned
  binary relations. Also, simple representation as a vector."
  (:require [kigen.pbr :as pbr]
            [kigen.sgp :as sgp]
            [kigen.pos :as pos]))

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

(defn cyclic-gen [n] [(ncycle n)])

(defn symmetric-gens
  "Generators of the symmetric group of degree n using the embedding
  into the partitioned binary relation monoid defined by f."
  [n]
  (cond (= 1 n) [[0]]
        (= 2 n) [(transposition n)]
        :else [(ncycle n) (transposition n)]))

(defn full-ts-gens
  "Generators of the full transformation semigroup of degree n."
  [n]
  (if (= 1 n)
    (symmetric-gens n)
    (concat (symmetric-gens n) [(collapsing n)])))

(defn pts-gens
  "Generators of the partial transformation semigroup of degree n."
  [n]
  (let [ftsg (full-ts-gens n)]
    (concat (map #(conj % n) ftsg)
            [(vec (concat [n] (range 1 n) [n]))])))

(defn sym-inv-gens
  "Generators of the symmetric inverse monoid of degree n."
  [n]
  (let [ftsg (symmetric-gens n)]
    (concat (map #(conj % n) ftsg)
            [(vec (concat [n] (range 1 n) [n]))])))


(defn mul
  "Right multiplication of transformations represented by vectors."
  [s t]
  (mapv t s)) ; as simple as that

(defn sgp-by-gens
  "Transformation semigroup by generators. "
  [gens]
  (sgp/sgp-by-gens gens mul))

(defn act
  [points t]
  (set (map t points)))

;;TODO bit of confusion, since this should in the permutation namespace,
;; but that is still PBR
(defn inverse
  "Inverse of a bijective transformation."
  [t]
  (mapv second
        (sort (map (fn [x] [(t x) x])
                   (range (count t))))))

(defn conjugate
  "The conjugate of a transformation by a permutation."
  [t p]
  (mul (mul (inverse p) t) p))

(defn conjugators
  "The conjugate of a transformation by a permutation."
  [t conjdt G]
  (filter #(= conjdt (conjugate t %)) G)
)

;; 'native' conjugacy class representative calculation
(defn single-maps
  "All mappings of of a transformation in the form of [src img]."
  [t]
  (map vector (range (count t)) t))

(defn realize-a-mapping ;;check individual map
  "m - mapping, d - desired mapping, p - current permutation bindings
  returns the extended p if possible, otherwise nil"
  [m d p]
  (let [nmappings (vec (distinct (map vector m d)))]
    (if (and
         (apply distinct? (map second nmappings))
         (every?
          (fn [[a b]]
            (or
             (and (contains? p a)
                  (= (p a) b))
             (and (not (contains? p a))
                  (empty? (filter #(= b %) (vals p))))))
          nmappings))
      (into p nmappings)
      nil)))

(defn all-realizations
  "All realizations of desired map d using available mappings, induced by p."
  [[mappings p] d]
  (reduce
   (fn [psols m]
     (let [res (realize-a-mapping m d p)]
       (if (nil? res)
         psols
         (conj psols [(disj mappings m) res]))))
   []
   mappings))

(defn conjrep [t]
  (let [n (count t)
        mappings (set (single-maps t))
        ;; [rep mappings pperm] contains a rep realized by a partially defined
        ;; permutation (as a map) and the available mappings not yet used
        stack (mapv (fn [i] [ [i] [ [mappings {}] ]]) (reverse (range n)))
        search (fn [stack]
                 (let [[rep psols] (peek stack)
                       nstack (pop stack)
                       pts (range n)
                       npsols (mapcat (fn [[mappings pperm]]
                                        (all-realizations [mappings pperm]
                                                          [(dec (count rep))
                                                           (peek rep)]))
                                      psols)]
                   (if (empty? npsols)
                     (recur nstack)
                     (if (and (= n (count rep)) true)
                       rep
                       (if (< (count rep) n)
                         (let [newreps (reverse (map #(conj rep %) pts))
                               newtasks (map (fn [rep] [rep npsols]) newreps)]
                           (recur (into nstack newtasks))))))))]
    (search stack)))
