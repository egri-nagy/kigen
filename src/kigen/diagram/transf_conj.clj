(ns kigen.diagram.transf-conj
  "'Native' conjugacy class representative calculation. Transformations are
  separated into single point mappings of the form [source image].
  A permutation is constructed by finding the minimal relabeling of a
  transformation."
  (:require   [kigen.diagram.transf :as t]
              [orbit.core :refer [tree-search]]
              [kigen.semigroup.conjugacy :as conjugacy]
              [clojure.set :refer [map-invert]]
              [clojure.data.int-map :refer [dense-int-set]]))

(defn single-maps
  "All mappings of a transformation in the form of [src img] extracted
  from a transformation t."
  [t]
  (map vector (range (count t)) t))

(defn realize-a-mapping
  "Given a mapping m and a desired mapping d, we try to turn the mapping m
  into d by extending a partial permutation p represented as a hashmap.
  This may fail if we already had a map to that point, or we end up mapping
  a single point to two images.
  An extended hashmap is returned if it is possible, otherwise nil."
  [m d p]
  (let [nmappings (distinct (map vector m d))]
    (when (and
           (apply distinct? (map second nmappings)) ;any contradicting maps?
           (every? (fn [[a b]]
                     (if (contains? p a)
                       (= (p a) b) ;if we have it, it should match
                       (empty? (filter (partial = b) ;or none should map to it
                                       (vals p)))))
                   nmappings))
      (into p nmappings))))

(defn all-realizations
  "All realizations of a desired map d using available mappings, compatible with
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
        initial_stack (mapv
                       (fn [i]  [ [] [ [mappings {}] ] i])
                       pts)
        search (fn [stack]
                 (let [[rep psols pt] (peek stack)
                       k (count rep)]
                   (if (= k n)
                     rep
                     (let [npsols (mapcat (fn [[mappings pperm]]
                                            (all-realizations mappings
                                                              pperm
                                                              [k pt]))
                                          psols)
                           ntasks (when (not-empty npsols)
                                    (for [np pts] [(conj rep pt) npsols np]))]
                       (recur (into (pop stack) ntasks))))))]
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
        solutions (tree-search [ [tmaps rmaps {}] ]
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
    (conjugacy/setconjrep t/conjugate T symmetries)))

(defn conj-conj
  "Conjoins a transformation to a conjugacy class representative
  sequence, based on the minimal conjugators of the sequence. "
  ([t] (let [r (conjrep t)]
         [ [r] (conjugators t r)]))
  ([[L G] t]
   (let [[r nG] (conjugacy/minconjugators t/conjugate t G)]
     [(conj  L r) nG])))

(defn setconjrepfunc ;todo: give it a better name
  "Returns a conjugacy representative set calculating function.
  This precalculates the minimal conjugators, hance the speed."
  [S G]
  (let [vS (vec (sort S)) ;todo: could relying on sorted elements be an issue?
        n (count vS)
        indices (vec (range n))
        t2i (map-invert (zipmap indices vS))
        ;; turning conjugation into simple action
        Ghom (fn [p] (mapv t2i (map #(t/conjugate % p) vS)))
        H (map Ghom G)
        cf (fn [x p] (p x))
        ;; mapping indices to the index of the conjugacy rep
        conjreps (zipmap indices
                         (map (fn [x] (t2i (conjrep (vS x))))
                              indices))
        ;; index to its minimal conjugators
        minconjs (zipmap indices
                         (map (fn [x] (set
                                       (second
                                        (conjugacy/minconjugators cf x H))))
                              indices))]
    ;; set-wise conjugacy class rep function for subsets of indices
    (fn [sub]
      (let [conjugators (reduce
                         (fn [[m mcjs :as r] x]
                           (let [xrep (conjreps x)
                                 flag (compare xrep m)]
                             (cond (neg? flag) [xrep (minconjs x)]
                                   (zero? flag) [m (into mcjs (minconjs x))]
                                   :else r)))
                         [(inc n) #{}] ;giving a max value to start
                         sub)]
        (dense-int-set (conjugacy/setconjrep cf (seq sub) (second conjugators)))))))
