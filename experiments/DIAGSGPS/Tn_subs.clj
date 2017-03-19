(def T3 (kigen.transf/sgp-by-gens
         (kigen.transf/full-ts-gens 3)))
(def mtT3 (kigen.multab/multab
           T3
           kigen.transf/mul))

(println (count (kigen.multab/subsgps mtT3)))
