;; constructing abstract semigroupoids
;; v25.06.xx
(require '[kigen.semigroupoid.enumeration :refer [semigroupoids-order-n]])
(require '[kigen.semigroupoid.homomorphism :refer [comptabs-up-to-morphisms]])

;; without endoarrows
(def S '[3 - -
         - 3 -
         - - 3])


;(doseq [sgpoid (comptabs-up-to-morphisms (semigroupoids-order-n 3))]
;  (println sgpoid))

;(println (count (semigroupoids-order-n 3)))

(def T '[4 - - -
         - 0 - -
         - - 1 -
         - - - 2])

(doseq [sgpoid (semigroupoids-order-n 4 T)]
  (println sgpoid))
