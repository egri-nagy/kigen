(require '[kigen.semigroupoid.homomorphism :refer [sgps-up-to-morphisms]])
(require '[kigen.semigroupoid.enumeration :refer [semigroups-order-n]])

(println (count (sgps-up-to-morphisms (semigroups-order-n 4))))