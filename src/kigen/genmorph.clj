(ns kigen.genmorph
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]))

(declare morph add-edge extend-node)

(def sortedvec (comp vec sort set))

(defn conjclass
  [l G]
  (sortedvec (map #(mapv (fn [x] (transf/conjugate x %))
                         l)
                  G)))

(defn conjrep-shadow [l G] (first (conjclass l G)))

(defn minconjugators [t G]
  (let [conjugations (map (fn [p] [(transf/conjugate t p) p]) G)
        mint (first (sort (map first conjugations)))]
    [mint  (map second (filter #(= mint (first %)) conjugations))]))

(defn conj-conj
  [[cl G] t]
  (let [[mint nG] (minconjugators t G)]
    [(conj cl mint) nG]))

(defn conjrep [l G]
  (first
    (reduce conj-conj [[] G] l)))

(defn setconjrep
  [coll G]
  (let [ccl (map #(vec (sort (mapv (fn [x] (transf/conjugate x %))
                                   coll)))
                 G)]
    (first (sortedvec ccl))))

                                        ;another conjrep
(defn conjrep? [l G] (= l (conjrep l G)))

(defn source
  "data items of source semigroup"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))
        gentab (vec (pmap
                     (fn [x] (->> gens
                                  (map #(mul x %))
                                  (map #(indices %))
                                  (vec)))
                     elts))]
    {:gens (range (count gens))
     :gentab gentab
     :elts elts
     :indices indices
     :mul (fn [x y] ((gentab x) y))
     :genips (map #(sgp/index-period % mul) gens)}))

(defn target
  "data items of target semigroup"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        classes (group-by #(sgp/index-period % mul) S)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))]
    {:gens gens
     :mul mul
     :elts elts
     :indices indices
     :classes classes}))

(defn targets [src trgt] (map #((:classes trgt) %)
                              (:genips src)))

(defn embedding-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt]
  (map (fn [l] (zipmap (:gens src) l))
       (apply cartesian-product (targets src trgt))))

(defn conj-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt G]
  (let [tgs (targets src trgt)]
    (map (fn [l] (zipmap (:gens src) l))
         (filter #(conjrep? % G)
                 (apply cartesian-product tgs)))))

(defn conj-seeds2
  [src trgt G]
  (let [tgs (targets src trgt)
        singletons (map #(conj-conj [[] G] %) (first tgs))
        seeds (loop [x  singletons
                     y (rest tgs)]
                (if (empty? y)
                  (map first x)
                  (let [X (cartesian-product x (first y))]
                    (recur (map (fn [[a b]] (conj-conj a b)) X)
                           (rest y)))))]
    (map (fn [l] (zipmap (:gens src) l)) (set seeds))))

(defn embeddings
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(morph % (:gens src)  (:gens src) (:mul src) (:mul trgt))
                         (embedding-seeds src trgt))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul G]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(morph % (:gens src) (:gens src) (:mul src) (:mul trgt))
                         (conj-seeds2 src trgt G))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching - next 3 functions are nested, top to bottom ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn morph
  "Extends the morphism if possible, otherwise number of matched elements."
  [phi front Sgens Smul Tmul]
  (loop [phi phi, stack (vec front)]
    (if (empty? stack)
      phi
      (let [result (extend-node phi (peek stack) Sgens Smul Tmul)]
        (if (number? result)
          result
          (recur (:phi result) (into (pop stack) (:new result))))))))

(defn extend-node
  "Extending a single element by all generators.
  Returns the updated morphism phi and the newly added nodes."
  [phi a gens Smul Tmul]
  (loop [phi phi, incoming [], gens gens]
    (if (empty? gens)
      {:phi phi :new incoming}
      (let [p (add-edge phi a (first gens) Smul Tmul)]
        (cond (number? p) p
              (empty? p) (recur phi
                                incoming
                                (rest gens))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest gens)))))))

(defn add-edge
  "Extends the morphism phi by applying a generator b to a single element a.
  phi - morphism represented as a map
  a - element to be extended
  b - generator of S
  It returns the number of matched elements so if it is not homomorphic,
  [] when homomorphic but no new element,
  [ab AB] where AB is the newly assigned image of the product ab."
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (if (= AB (phi ab))
        []
        (count phi))
      [ab AB])))
