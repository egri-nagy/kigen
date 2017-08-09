(ns kigen.transf
  "Transformations and permutations. Several embeddings into partitioned
  binary relations. Also, simple representation as a vector."
  (:require [kigen.sgp :as sgp]
            [orbit.core :as o]
            [kigen.conjugacy :as conjugacy]))

(declare single-maps)

(defn singleton? [coll] (= 1 (count coll)))

;; STANDARD GENERATING SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn idmap [n] (vec (range n)))
(defn transposition [n] (vec (concat [1 0] (range 2 n))))
(defn ncycle [n] (vec (concat (range 1 n) [0])))
(defn collapsing [n] (vec (concat [0 0] (range 2 n))))

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
  "Transformation t acting on a set of points."
  [points t]
  (set (map t points)))

;;TODO bit of confusion, since this should in the permutation namespace,
;; but that is still PBR
(defn inverse
  "Inverse of a bijective transformation."
  [t]
  (let [pts (range (count t))]
    (mapv (zipmap t pts) pts)))

(defn conjugate-by-definition
  "The conjugate of a transformation by a permutation according to the
  definition, i.e. multiplying by inverse on the left and p on the right."
  [t p]
  (mul (mul (inverse p) t) p))

(defn conjugate
  "The conjugate of a transformation by direct relabeling according to p."
  [t p]
  (let [pts (range (count t))]
    (mapv (zipmap (map p pts) (map p t)) pts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'native' conjugacy class representative calculation ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformations are separated into single point mappings. A permutation is
;; constructed by finding the minimal relabeling of a transformation.

(defn single-maps
  "All mappings of of a transformation in the form of [src img]."
  [t]
  (map vector (range (count t)) t))

(defn realize-a-mapping
  "Given a mapping m and a desired mapping d, we try to turn m into d by
  extending a partial permutation p represented as a hashmap.
  An extended hashmap is returned if it is possible, otherwise nil."
  [m d p]
  (let [nmappings (distinct (map vector m d))]
    (when (and
           (or
            (= 1 (count nmappings))
            (apply distinct? (map second nmappings)))
           (every?
            (fn [[a b]]
              (if (contains? p a)
                (= (p a) b)
                (empty? (filter (partial = b) (vals p)))))
            nmappings))
      (into p nmappings))))

(defn all-realizations
  "All realizations of desired map d using available mappings, compatible with
  partial permutation p. Just systematically trying to realize all mappings."
  [mappings p d]
  (reduce
   (fn [psols m]
     (let [res (realize-a-mapping m d p)]
       (if (nil? res)
         psols
         (conj psols [(disj mappings m) res]))))
   []
   mappings))

(defn conjrep
  "Direct construction of conjugacy class representative of transformation t."
  [t]
  (let [n (count t)
        pts (range n)
        mappings (set (single-maps t))
        ;; [rep mappings pperm] contains a rep realized by a partially defined
        ;; permutation (as a map) and the available mappings not yet used
        queue (into clojure.lang.PersistentQueue/EMPTY
                    (map (fn [i]
                           [ [i] [ [mappings {}] ] ])
                         pts))
        search (fn [queue]
                 (let [[rep psols] (peek queue)
                       nqueue (pop queue)
                       npsols (mapcat (fn [[mappings pperm]]
                                        (all-realizations mappings pperm
                                                          [(dec (count rep))
                                                           (peek rep)]))
                                      psols)]
                   (if (empty? npsols)
                     (recur nqueue)
                     (if (and (= n (count rep)) true)
                       rep
                       (if (< (count rep) n)
                         (let [newreps (map #(conj rep %) pts)
                               newtasks (map (fn [rep] [rep npsols]) newreps)]
                           (recur (into nqueue newtasks))))))))]
    (search queue)))

(defn conjugators
  "All permutations that take t to r by conjugation."
  [t r]
  (let [tmaps (set (single-maps t))
        rmaps (set (single-maps r))
        f (fn [[tmaps rmaps perm]]
            (let [tm (first tmaps)]
              (set (for [rm rmaps
                         :let [res (realize-a-mapping tm rm perm)]
                         :when (not (nil? res))]
                     [(rest tmaps) (disj rmaps rm) res]))))]
    (map #(mapv % (range (count t))) ;creating the permutation vector
         (map (fn [[_ _ perm]] perm) ;extracting the map
              (o/acyclic-search-bulk [ [tmaps rmaps {}] ]
                                     f
                                     #(empty? (first %)))))))

;; conjrepfunc could be t/conjrep
;; or
;; (conjugacy/conjrep conjugate t syms)
(defn min-rep-and-class
  "Finds the minimal conjugacy class representative and its class
  using the given function for calculating representatives in the
  collection T."
  [T conjrepfunc]
  (reduce
   (fn [[m mc :as db] t]
     (let [r (conjrepfunc t)
           flag (compare r m)]
       (cond (neg? flag) [r [t]]
             (zero? flag) [m (conj mc t)]
             :else db)))
   [(conjrep (first T)) [(first T)]]
   (rest T)))

(defn syms [[minimal minclass]]
  (distinct (mapcat
             #(conjugators % minimal)
             minclass)))

(defn nxt
  [[minimal conjclass elts]]
  (map #(min-rep-and-class
         (disj elts %)
         (fn [t]
           (conjugacy/conjrep
            conjugate
            t
            (conjugators % minimal))))
       conjclass))


(defn setconjrep
  "Setwise conjugacy class representative of T.
  Using conjugacy/setconjrep, but only with symmetries that take
  produce the minimal conjrep."
  [T]
  (let [[minimal minclass] (min-rep-and-class T conjrep)
        symmetries (distinct (mapcat
                              #(conjugators % minimal)
                              minclass))]
    (conjugacy/setconjrep conjugate T symmetries)))

(defn conj-conj
  "Conjoins a transformation to a conjugacy class representative
  sequence, based on the minimal conjugators of the sequence. "
  ([t] (let [r (conjrep t)]
         [ [r] (conjugators t r)]))
  ([[L G] t]
   (let [[r nG] (conjugacy/minconjugators conjugate t G)]
     [(conj  L r) nG])))
