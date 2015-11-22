(ns kigen.sgp
  (:use [kigen.orbit :only [bfs actions action-function]]))

;; semigroup by generators
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  [gens mul]
  (bfs gens (action-function (actions gens mul))))
