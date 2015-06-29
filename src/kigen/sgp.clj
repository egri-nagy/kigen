(ns kigen.sgp
  (:use [kigen.orbit :only [alternating-orbit actions]]))

;; semigroup by generators
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  [gens mul]
  (alternating-orbit gens (actions gens mul))) 
