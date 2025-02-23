;;; just to test a couple of relational morphisms
(require '[kigen.transf :refer [full-ts-gens symmetric-gens mul]])
(require '[kigen.sgp :refer [sgp-by-gens]])
(require '[kigen.multab :refer [multab]])
(require '[kigen.multab-morphism :refer [relmorphisms
                                         divisions
                                         homomorphisms
                                         isomorphisms]])

(def T2 (sgp-by-gens (full-ts-gens 2) mul))
(def mtT2 (multab T2 mul))

(def T3 (sgp-by-gens (full-ts-gens 3) mul))
(def mtT3 (multab T3 mul))


(def S3 (sgp-by-gens (symmetric-gens 3) mul))
(def mtS3 (multab S3 mul))

(relmorphisms mtT2 mtT2)
(divisions mtT2 mtT2)

(relmorphisms mtS3 mtS3)
(divisions mtS3 mtS3)

(isomorphisms mtS3 mtS3)

(def S13 (sgp-by-gens  [[2 1 0] [1 0 0]] mul))
(def mtS13 (multab S13 mul))

(println  (count (divisions mtT2 mtS13)))