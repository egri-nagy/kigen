(ns kigen.diagsgp
  (:require [kigen.pbr :as pbr])
  (:require [kigen.sgp :as sgp])
  (:require [kigen.transf :as transf]))

(defn full-ts
  "Full transformation monoid of degree n."
  [n]
  (sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))
