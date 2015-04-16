(ns kigen.sgp)

;; semigroup by generators TODO maybe a general orbit algorithm will do
;; gens - generator elements
;; rightmul - right multiplication
(defn sgp-by-gens
  [gens rightmul]
  (let [funcs (for [x gens] #(rightmul % x))]
    funcs))
