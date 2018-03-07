(ns kigen.transf-conj
  "Transformations and permutations simply representated as vectors."
  (:require [kigen.sgp :as sgp]
            [kigen.transf :as t]
            [orbit.core :refer [tree-search]]
            [kigen.conjugacy :as conjugacy]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'native' conjugacy class representative calculation ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformations are separated into single point mappings. A permutation is
;; constructed by finding the minimal relabeling of a transformation.

(defn single-maps
  "All mappings of a transformation in the form of [src img]."
  [t]
  (map vector (range (count t)) t))

(defn realize-a-mapping
  "Given a mapping m and a desired mapping d, we try to turn m into d by
  extending a partial permutation p represented as a hashmap.
  An extended hashmap is returned if it is possible, otherwise nil."
  [m d p]
  (let [nmappings (distinct (map vector m d))]
    (when (and
           (apply distinct? (map second nmappings))
           (every? (fn [[a b]]
                     (if (contains? p a)
                       (= (p a) b)
                       (empty? (filter (partial = b)
                                       (vals p)))))
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
        initial_stack (mapv (fn [i]
                              [ [] [ [mappings {}] ] i])
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
