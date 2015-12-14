(ns kigen.sgp
  (:use [kigen.orbit :only [bfs actions action-function]])
  (:require [kigen.pbr :as pbr])
  (:require [clojure.math.combinatorics :as combinatorics]))

;; semigroup by generators
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  ([gens] (sgp-by-gens gens pbr/mul))
  ([gens mul]
   (bfs gens (action-function (actions gens mul)))))

(defn commutative? [sgp]
  (every? #(= (pbr/mul (first %) (second %)) (pbr/mul (second %) (first %)))
          (combinatorics/combinations sgp 2)))
