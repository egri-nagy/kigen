(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]))

(declare morph extend-by-gen extend-by-all-gens)

;; cli dump for getting conjreps of seeds



(defn conjseq [coll p] (mapv #(transf/conjugate % p) coll))

(defn conjclass [l G] (vec (set (map #(conjseq l %) G))))

(defn conjrep? [l G] (= l (first (sort (conjclass l G)))))

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

(defn embedding-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt]
  (map (fn [l] (zipmap (:gens src) l))
       (apply cartesian-product (map
                                 #((:classes trgt) %)
                                 (:genips src)))))

(defn embedding-seeds-conj
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt G]
  (map (fn [l] (zipmap (:gens src) l))
       (filter #(conjrep? % G)
               (apply cartesian-product (map
                                         #((:classes trgt) %)
                                         (:genips src))))))

(defn embeddings
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(morph % (:mul src) (:mul trgt))
                         (embedding-seeds src trgt))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul G]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(morph % (:mul src) (:mul trgt))
                         (embedding-seeds-conj src trgt G))))))


;; Cayley graph morph matching
(defn morph
  "Extends the given morphism if possible, otherwise nil."
  [phi Smul Tmul]
  (let [gens (keys phi)]
    (loop [phi phi, stack (vec gens)]
      (if (empty? stack)
        phi
        (let [result (extend-by-all-gens phi (peek stack) gens Smul Tmul)]
          (if (number? result)
            result
            (recur (:phi result) (into (pop stack) (:new result)))))))))

(defn extend-by-all-gens
  "Extending a single element by all generators.
  Returns the updated morphism phi and the newly added elements."
  [phi a gens Smul Tmul]
  (loop [phi phi, incoming [], gens gens]
    (if (empty? gens)
      {:phi phi :new incoming}
      (let [p (extend-by-gen phi a (first gens) Smul Tmul)]
        (cond (number? p) p
              (empty? p) (recur phi
                                incoming
                                (rest gens))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest gens)))))))

(defn extend-by-gen
  "Extends a single element of S by a single generator.
  phi - morphism represented as a map
  a - element to be extended
  b - generator of S
  returns nil if it is not homomorphic, [] when homomorphic but no new element,
  [ab AB] where AB is the newly assigned image of the product ab"
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (if (= AB (phi ab)) [] (count phi))
      [ab AB])))
