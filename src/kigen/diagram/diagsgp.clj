(ns kigen.diagram.diagsgp
  (:require [kigen.diagram.pbr :as pbr]
            [kigen.semigroup.sgp :as sgp]
            [kigen.diagram.transf :as transf]))

(defn full-ts
  "Full transformation monoid of degree n."
  [n]
  (sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))

(defn full-pbr1
  []
  (sgp/sgp-by-gens
   (map pbr/ext->int
        [ [ [[-1] ] [ [1]] ]
          [ [[-1] ] [ []] ]
          [ [[] ] [ [1]] ]
          [ [[1 -1] ] [ [1]] ]
          [ [[-1] ] [ [1 -1]] ] ])
   pbr/mul))

(defn full-pbr2
  []
  (sgp/sgp-by-gens
   (map pbr/ext->int
        [
         [  [  [  ], [ -1 ]  ], [ [ 2 ], [ -2, 1 ] ] ]
         [   [ [ -2, 1 ], [ -1 ] ], [ [ 2 ], [  ] ] ]
         [ [ [ -1, 2 ], [ -2 ] ], [ [ 1 ], [ 2 ] ] ]
         [ [ [ -1 ], [ -2 ] ], [ [ 1 ], [ -2, 2 ] ]]
         [ [ [ -2 ], [ 2 ] ], [ [ 1 ], [ 2 ] ]]
         [ [ [ -2 ], [ -1 ] ], [ [ 1 ], [ 1, 2 ] ]]
         [ [ [ -2 ], [ -1 ] ], [ [ 1 ], [ 2 ] ]]
         [ [ [ -2 ], [ -1 ] ], [ [ 1 ], [ -2 ] ]]
         [ [ [ -2 ], [ -1 ] ], [ [ 2 ], [ 1 ] ]]
         [ [ [ -2 ], [ -2, -1 ] ], [ [ 1 ], [ 2 ] ]]
         ])
   pbr/mul))

(defn full-binrel2
  []
  (sgp/sgp-by-gens
   (map pbr/ext->int
        [
         [  [  [ -2 ], [ -1 ]  ], [ [  ], [  ] ] ]
         [   [ [ -1 ], [  ] ], [ [  ], [  ] ] ]
         [ [ [ -1, -2 ], [ -1 ] ], [ [  ], [  ] ] ]
         [ [ [ -1,-2 ], [ ] ], [ [  ], [ ] ]]
         ])
   pbr/mul))
