(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset? difference]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare sxt m)


(defn f [phi Smul Tmul]
  (m {:phi phi :stack (vec (keys phi))} (keys phi) Smul Tmul))

;; single element extended by all generators
(defn m [env gens Smul Tmul]
  (let [st (:stack env)
        phi (:phi env)]
    (if (empty? st)
      phi
      (let [l (map
               #(sxt phi (peek st)  % Smul Tmul)
               gens)]
        (if (some nil? l)
          nil
          (do
(println phi st)
            
            (recur {:phi (into phi (remove empty? l)) :stack (pop st)} gens Smul Tmul)))))))

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


