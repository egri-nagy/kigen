;; Recomputing all subsemigroups of T_3 up to conjugacy.

(load-file "../subsgps_minimal_gensets.clj")

(def T3 (t/sgp-by-gens (t/full-ts-gens 3)))
(def S3 (t/sgp-by-gens (t/symmetric-gens 3)))

(subsgps (t/sgp-by-gens T3) S3 map)



