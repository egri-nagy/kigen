(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]))

(declare morph extend-by-gen extend-by-all-gens)


(defn lossless-morph-seeds
  "Given a generator set this returns the sequence of all possible morphism
  seeds (meaning that they are index-period checked)."
  [Sgens T Smul Tmul]
  (let [classes (group-by #(sgp/index-period % Tmul) T)]
    (map (fn [l] (zipmap Sgens l))
         (apply cartesian-product (map #(classes (sgp/index-period % Smul))
                                       Sgens)))))

(defn isoms [Sgens T Smul Tmul]
  (filter #(apply distinct? (vals %))
          (remove nil?
                  (map #(morph % Smul Tmul)
                       (lossless-morph-seeds Sgens T Smul Tmul)))))

;; Cayley graph morph matching
(defn morph
  "Extends the given morphism if possible, otherwise nil."
  [phi Smul Tmul]
  (let [gens (keys phi)
        img (set (vals phi))]
    (loop [phi (conj phi [:img img]), stack (vec gens)]
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
              :else (recur (update-in (conj phi p) [:img] #(conj % (second p)))
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
      (if (= AB (phi ab))
        []
        nil)
      (if (contains? (:img phi) AB)
        nil
        [ab AB]))))
