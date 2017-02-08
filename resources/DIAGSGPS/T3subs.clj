(def T3 (kigen.sgp/sgp-by-gens (kigen.transf/full-ts-gens 3)))
(def mtT3 (kigen.multab/multab T3))

(println (kigen.multab/subsgps mtT3))
