(require '[kigen.semigroupoid.homomorphism :refer [comptabs-up-to-morphisms]])
(require '[kigen.semigroupoid.enumeration
           :refer [semigroups-order-n
                   associativity?]])

;this gets out of memory error with 48G of ram
;(println (count (sgps-up-to-morphisms (semigroups-order-n 4))))

(println (every? associativity? (semigroups-order-n 4 comptab)))

(semigroups-order-n 2 '[1 - - -])
