;; from SGPDEC/examples/namedsemigroups.g
(def becks
  [[1,2,3,1,1,1], ;creates the image {1,2,3}
   [4,4,4,5,4,6], ;transposition in {4,5,6}
   [4,4,4,5,6,4], ;cycle of {4,5,6}
   [4,4,4,4,5,5], ;this and the nontrivial holonomy group of
                  ;{4,5,6} generate the images with cardinality 2
   [4,4,4,1,2,3], ;this maps {4,5,6} to {1,2,3}
   [2,3,1,4,4,4]]);makes H({1,2,3}) nontrivial

(def BECKS (kigen.sgp/sgp-by-gens
            (map kigen.transf/transf->binrel becks)
            kigen.pbr/mul))

(def alifex [ [ 2, 2, 3, 3, 3 ], [ 3, 3, 3, 5, 4 ] ])
