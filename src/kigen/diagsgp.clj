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
   [{ :dom #{1} :cod #{2} 1 #{2} 2 #{1}}
    { :dom #{1} :cod #{2} 1 #{2} 2 #{}}
    { :dom #{1} :cod #{2} 1 #{} 2 #{1}}
    { :dom #{1} :cod #{2} 1 #{1 2} 2 #{1}}
    { :dom #{1} :cod #{2} 1 #{2} 2 #{1 2}}]
   pbr/mul))
