(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as t])
(require '[kigen.diagram.transf-conj :as t-c])
(require '[kigen.semigroup.conjugacy :as c])
(require '[kigen.canonical-labeling :refer [can-set-seq]])

(def S5 (sgp-by-gens (t/symmetric-gens 5) t/mul))
(def sets [[[1 2 1 2 1] [3 3 1 2 0] [4 1 3 0 2]]
            [[3 3 1 2 0] [4 1 3 0 2]]
            [[1 2 1 2 1] [0 0 1 2 0] [4 1 3 0 2] [1 2 1 2 1] [0 1 0 1 0]]])

      
 (t-c/setconjrep (nth sets 0))
 (c/setconjrep t/mul (nth sets 0) S5)
 (can-set-seq (nth sets 0))
         
