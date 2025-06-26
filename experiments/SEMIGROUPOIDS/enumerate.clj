;; constructing abstract semigroupoids
;; v25.06.xx
(require '[kigen.semigroupoid.enumeration :refer [semigroupoids-order-n]])

(def S '[0 - -
         - 0 -
         - - 0])	

(doseq [sgpoid (semigroupoids-order-n 3 S)]
  (println sgpoid))

;(println (count (semigroupoids-order-n 3)))