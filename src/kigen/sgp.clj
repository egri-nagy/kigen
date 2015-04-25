(ns kigen.sgp
  (:use kigen.orbit))

;; semigroup by generators
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  [gens mul]
  (orbit gens ;generators are already known elements
         (for [x gens] #(mul % x)))) ;right multiplication functions
