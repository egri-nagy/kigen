(def T3 (kigen.sgp/sgp-by-gens
         (kigen.transf/full-ts-gens 3)
         kigen.transf/mul))
(def mtT3 (kigen.multab/multab
           T3
           kigen.transf/mul))

(println (kigen.multab/subsgps mtT3))
