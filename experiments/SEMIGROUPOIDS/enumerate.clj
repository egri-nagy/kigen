;; constructing abstract semigroupoids
;; v25.06.xx
(require '[kigen.semigroupoid.enumeration :refer [semigroupoids-order-n]])

(def S '[3 - -
         - 3 -
         - - 3])	

(def S2 '[4 - - -
          - 4 - -
          - - 4 -
          - - - 4])	

(doseq [sgpoid (semigroupoids-order-n 4 S2)]
  (println sgpoid))

;(println (count (semigroupoids-order-n 3)))