;; Calculating all subsemigroups of K_{4,2} up to conjugacy.
;; It takes about 13 minutes on an Apple M1 processor and it is possible
;; to compute it on a Raspberry Pi 4, thus it is good for benchmarking
;; multi-core efficiency.

(load-file "experiments/SUBSEMI/subsgps_minimal_gensets.clj")

(def K42 [ [ 0, 1, 1, 1 ],
          [ 0, 0, 2, 2 ],
          [ 2, 0, 0, 2 ],
          [ 3, 1, 3, 3 ],
          [ 2, 2, 0, 2 ],
          [ 0, 3, 0, 3 ],
          [ 2, 2, 3, 2 ],
          [ 1, 1, 2, 2 ],
          [ 2, 2, 2, 0 ] ])

(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))
(subsgps (t/sgp-by-gens K42) S4 pmap)
