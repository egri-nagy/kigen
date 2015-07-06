(ns kigen.sgp
  (:use [kigen.orbit :only [orbit actions]]))

;; semigroup by generators
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  [gens mul]
  (orbit gens (actions gens mul))) 
