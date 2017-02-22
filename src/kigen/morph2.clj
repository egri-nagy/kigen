(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset? difference]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare morph extend-by-gen extend-by-all-gens)

(defn morph [phi Smul Tmul]
  (let [gens (keys phi)]
    (loop [env {:phi phi :stack (vec gens)}]
      (if (empty? (:stack env))
        (:phi env)
        (let [nenv (extend-by-all-gens env (peek (:stack env)) gens Smul Tmul)]
          (if (nil? nenv)
            nil
            (recur {:phi (:phi nenv) :stack (into (pop (:stack env)) (:stack nenv))})))))))

(defn extend-by-all-gens [env a gens Smul Tmul]
  (let [phi (:phi env)
        stack []]
    (if (empty? gens)
      env
      (let [p (extend-by-gen phi a (first gens) Smul Tmul)]
        (cond (nil? p) nil
              (empty? p) (recur {:phi phi :stack stack} a  (rest gens) Smul Tmul)
              :else (recur {:phi (conj phi p) :stack (conj stack (first p))} a (rest gens) Smul Tmul))))))

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
      [ab AB])))


