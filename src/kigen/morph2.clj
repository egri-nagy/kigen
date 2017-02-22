(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset? difference]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare sxt extendt)

(defn f [phi Smul Tmul]
  (let [gens (keys phi)]
    (loop [env {:phi phi :stack (vec gens)}]
      (if (empty? (:stack env))
        (:phi env)
        (let [nenv (extendt env (peek (:stack env)) gens Smul Tmul)]
          (if (nil? nenv)
            nil
            (recur {:phi (:phi nenv) :stack (into (pop (:stack env)) (:stack nenv))})))))))

(defn extendt [env a gens Smul Tmul]
  (let [phi (:phi env)
        stack []]
    (if (empty? gens)
      env
      (let [p (sxt phi a  (first gens) Smul Tmul)]
        (cond (nil? p) nil
              (empty? p) (recur {:phi phi :stack stack} a  (rest gens) Smul Tmul)
              :else (recur {:phi (conj phi p) :stack (conj stack (first p))} a (rest gens) Smul Tmul))))))

;; single element extended by a single generator
(defn sxt
  "phi - morphism represented as a map
  a - element in frontline
  b - generator of S"
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (if (= AB (phi ab))
        []
        nil)
      [ab AB])))


