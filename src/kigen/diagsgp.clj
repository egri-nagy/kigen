(ns kigen.diagsgp
  (:require [kigen.pbr :as pbr])
  (:require [kigen.sgp :as sgp])
  (:require [kigen.transf :as transf]))

(defn full-ts
  "Full transformation monoid of degree n."
  [n]
  (sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))

(defn full-pbr1
  []
  (sgp/sgp-by-gens
   (map pbr/ext->int
        [ [ [[-1] ] [ [1]] ]
          [ [[-1] ] [ []] ]
          [ [[] ] [ [1]] ]
          [ [[1 -1] ] [ [1]] ]
          [ [[-1] ] [ [1 -1]] ] ])
   pbr/mul))
