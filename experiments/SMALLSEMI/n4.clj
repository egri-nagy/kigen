(require '[kigen.semigroupoid.homomorphism :refer [sgps-up-to-morphisms]])
(require '[kigen.semigroupoid.enumeration :refer [semigroups-order-n2]])

;this gets out of memory error with 48G of ram
;(println (count (sgps-up-to-morphisms (semigroups-order-n 4))))

(doseq [sgp (semigroups-order-n2 4)]
  (println sgp))
