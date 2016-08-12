(ns kigen.sgp
  (:use [kigen.orbit :only [bfs right-actions set-action]])
  (:require [kigen.pbr :as pbr])
  (:require [clojure.math.combinatorics :as combinatorics]))

;; semigroup by generators
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  ([gens] (sgp-by-gens gens pbr/mul))
  ([gens mul]
   (bfs gens (set-action (right-actions mul gens)))))

(defn commutative? [sgp]
  (every? #(= (pbr/mul (first %) (second %)) (pbr/mul (second %) (first %)))
          (combinatorics/combinations sgp 2)))
