(require '[kigen.semigroupoid.homomorphism :refer [sgps-up-to-morphisms]])
(require '[kigen.semigroupoid.enumeration :refer [semigroups-order-n]])

;this gets out of memory error with 48G of ram
;(println (count (sgps-up-to-morphisms (semigroups-order-n 4))))

(def comptab '[3 3 3 3
               1 - - -
               1 - - -
               - - - -])

(doseq [sgp (semigroups-order-n 4 comptab)]
  (println sgp))

