;; Recomputing all subsemigroups of T_4 up to conjugacy.

(load-file "../subsgps_minimal_gensets.clj")

(def T4 (t/sgp-by-gens (t/full-ts-gens 4)))
(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))

(subsgps (t/sgp-by-gens T4) S4 pmap)
;;(subsgps (t/sgp-by-gens T4) S4 pmap (load-layer "layer007") (load-db "db007") 8 14126090)


