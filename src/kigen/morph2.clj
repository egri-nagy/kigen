(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]))

(declare morph extend-by-gen extend-by-all-gens)

(defn gentab
  "part of the multiplication table only with generators and the elements in
  order, generators in front"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        classes (group-by #(sgp/index-period % mul) S)
        genset (set gens)
        elts (vec (concat gens (remove genset S)))
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
     :classes (into {}
                    (map (fn [[key val]] [key (map indices val)]) classes))
     :genmul (fn [x y] ((gentab x ) y))}))

(defn embedding-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [Sgentab Tgentab]
  (let [Sgenips (map #(sgp/index-period % (:genmul Sgentab)) (:gens Sgentab))]
    (println Sgenips)
    (map (fn [l] (zipmap (:gens Sgentab) l))
         (apply cartesian-product (map
                                   #((:classes Tgentab) %)
                                   Sgenips)))))

(defn embeddings
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Tgens Smul Tmul]
  (let [Sgentab (gentab Sgens Smul)
        Tgentab (gentab Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove nil?
                    (map #(morph % (:genmul Sgentab) (:genmul Tgentab))
                         (embedding-seeds Sgentab Tgentab))))))

;; Cayley graph morph matching
(defn morph
  "Extends the given morphism if possible, otherwise nil."
  [phi Smul Tmul]
  (let [gens (keys phi)]
    (loop [phi phi, stack (vec gens)]
      (if (empty? stack)
        phi
        (let [result (extend-by-all-gens phi (peek stack) gens Smul Tmul)]
          (if (nil? result)
            nil
            (recur (:phi result) (into (pop stack) (:new result)))))))))

(defn extend-by-all-gens
  "Extending a single element by all generators.
  Returns the updated morphism phi and the newly added elements."
  [phi a gens Smul Tmul]
  (loop [phi phi, incoming [], gens gens]
    (if (empty? gens)
      {:phi phi :new incoming}
      (let [p (extend-by-gen phi a (first gens) Smul Tmul)]
        (cond (nil? p) nil
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
      (if (= AB (phi ab)) [] nil)
      [ab AB])))
