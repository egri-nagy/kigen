;; from SGPDEC/examples/namedsemigroups.g
(def becks
  [[0,1,2,0,0,0], ;creates the image {0,1,2}
   [3,3,3,4,3,5], ;transposition in {3,4,5}
   [3,3,3,4,5,3], ;cycle of {3,4,5}
   [3,3,3,3,4,4], ;this and the nontrivial holonomy group of
                  ;{3,4,5} generate the images with cardinality 1
   [3,3,3,0,1,2], ;this maps {3,4,5} to {0,1,2}
   [1,2,0,3,3,3]]);makes H({0,1,2}) nontrivial

(def alifex [ [ 1, 1, 2, 2, 2 ], [ 2, 2, 2, 4, 3 ] ])
