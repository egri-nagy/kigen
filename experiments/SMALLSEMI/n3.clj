(require '[kigen.semigroupoid.homomorphism :refer [comptabs-up-to-morphisms]])
(require '[kigen.semigroupoid.enumeration :refer [semigroups-order-n]])

;; this calculation is immediate

(def sgps3 (semigroups-order-n 3))

(println "Total: " (count sgps3))

(println "Up isom and anti-isom:"
         (count (comptabs-up-to-morphisms sgps3)))
