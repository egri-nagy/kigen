;; from SGPDEC/examples/namedsemigroups.g
(def becks
  [[0,1,2,0,0,0], ;creates the image {0,1,2}
   [3,3,3,4,3,5], ;transposition in {3,4,5}
   [3,3,3,4,5,3], ;cycle of {3,4,5}
   [3,3,3,3,4,4], ;this and the nontrivial holonomy group of
                  ;{3,4,5} generate the images with cardinality 1
   [3,3,3,0,1,2], ;this maps {3,4,5} to {0,1,2}
   [1,2,0,3,3,3]]);makes H({0,1,2}) nontrivial

(def bex
  [[0 1 0 0] [3 3 3 2] [2 2 3 3] [3 3 0 1] [1 0 3 3]])

(def alifex [ [ 1, 1, 2, 2, 2 ], [ 2, 2, 2, 4, 3 ] ])

;; overlapping covers for parallel components
(def overlapping-covers [[3 2 3 1] [0 1 1 0] [2 1 0 0]])

(def hey-bug [[1 1 3 3 5 5 7 7] [0 1 2 4 4 5 6 4] [0 1 2 3 2 3 2 3]])
(def micro-bug [[0 1 0] [2 2 0]])
