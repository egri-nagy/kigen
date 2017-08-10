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
        pts (reverse (range n)) ;we look for the minimal one and use stack
        mappings (set (single-maps t))
        ;;a task is a vector: [partial_rep seq_of_partial_solutions pt]
        ;;a partial solution is a pair of available mappings and the
        ;;corresponding partial permutation
        initial_stack (into []
                            (map (fn [i]
                                   [ [] [ [mappings {}] ] i])
                                 pts))
        search (fn [stack]
                 (let [[rep psols pt] (peek stack)
                       k (count rep)]
                   (if (= k n)
                     rep
                     (let [nstack (pop stack)
                           npsols (mapcat (fn [[mappings pperm]]
                                            (all-realizations mappings
                                                              pperm
                                                              [k pt]))
                                          psols)
                           ntasks (when (not-empty npsols)
                                    (for [np pts] [(conj rep pt) npsols np]))]
                       (recur (into nstack ntasks))))))]
    (search initial_stack)))

(defn conjugators
  "All permutations that take t to r by conjugation."
  [t r]
  (let [tmaps (set (single-maps t))
        rmaps (set (single-maps r))
        extend (fn [[tmaps rmaps perm]] ; extending a partial solution
                 (let [tm (first tmaps)]
                   (set (for [rm rmaps
                              :let [res (realize-a-mapping tm rm perm)]
                              :when (not (nil? res))]
                          [(rest tmaps) (disj rmaps rm) res]))))
        solutions (o/acyclic-search-bulk [ [tmaps rmaps {}] ]
                                         extend
                                         #(empty? (first %)))]
    (map (fn [perm]
           (mapv perm (range (count t)))) ;creating the permutation vector
         (map (fn [[_ _ perm]] perm) ;extracting the permutation (as hashmap)
              solutions))))

(defn setconjrep
  "Setwise conjugacy class representative of T.
  Using conjugacy/setconjrep, but only with symmetries that take
  produce the minimal conjrep."
  [T]
  (let [[minimal minclass] (conjugacy/min-rep-and-class T conjrep)
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
