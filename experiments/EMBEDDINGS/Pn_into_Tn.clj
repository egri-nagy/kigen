(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens index-period]])
(require '[kigen.diagram.transf :as t])
(require '[kigen.diagram.pbr :as pbr])
(require '[kigen.diagram.transf-conj :as t-c])
(require '[kigen.semigroup.conjugacy :as c])
(require '[kigen.semigroup.genmorph :refer [sgp-embeddings-by-gens class-reps call-embedding index-period-matched embedding-backtrack]])
(require '[kigen.canonical-labeling :refer [can-set-seq can-seq]])
(require '[kigen.table.multab :as mt])
(require '[kigen.table.multab-morphism :as mtm])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[taoensso.timbre :refer [trace set-min-level!]])
(require '[clojure.set :refer [union]])
(require '[clojure.pprint :refer [pprint]])

(set-min-level! :trace)
;(set-min-level! :debug)

;from GAP: Rows(MulTab(PartitionMonoid(2)));
(def P2multaborig
  [[1, 2, 3, 4, 5, 1, 1, 2, 1, 1, 2, 3, 3, 4, 5],
   [1, 2, 3, 4, 5, 1, 2, 2, 3, 4, 5, 3, 5, 4, 5],
   [1, 2, 3, 4, 5, 4, 3, 5, 2, 1, 2, 5, 3, 4, 5],
   [4, 5, 5, 4, 5, 4, 4, 5, 4, 4, 5, 5, 5, 4, 5],
   [4, 5, 5, 4, 5, 4, 5, 5, 5, 4, 5, 5, 5, 4, 5],
   [6, 8, 12, 14, 15, 6, 6, 8, 6, 6, 8, 12, 12, 14, 15],
   [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
   [6, 8, 12, 14, 15, 6, 8, 8, 12, 14, 15, 12, 15, 14, 15],
   [1, 2, 3, 4, 5, 10, 9, 11, 7, 6, 8, 13, 12, 14, 15],
   [10, 11, 13, 14, 15, 10, 10, 11, 10, 10, 11, 13, 13, 14, 15],
   [10, 11, 13, 14, 15, 10, 11, 11, 13, 14, 15, 13, 15, 14, 15],
   [6, 8, 12, 14, 15, 14, 12, 15, 8, 6, 8, 15, 12, 14, 15],
   [10, 11, 13, 14, 15, 14, 13, 15, 11, 10, 11, 15, 13, 14, 15],
   [14, 15, 15, 14, 15, 14, 14, 15, 14, 14, 15, 15, 15, 14, 15],
   [14, 15, 15, 14, 15, 14, 15, 15, 15, 14, 15, 15, 15, 14, 15]])

;decrementing by 1
(def P2multab
  (mapv (partial mapv dec) P2multaborig))


(defn P2mul
  [a b]
  (nth (nth P2multab a) b))

(def TP2gensorig [[1,2,3,5,4,6,7],
                  [2,2,4,4,6,6,6],
                  [3,2,3,3,3,7,7]])
(def TP2gens (mapv (partial mapv dec) TP2gensorig))

;this should be 7 according to https://arxiv.org/abs/2411.14693
(embedding-backtrack
 (range 15) P2mul
 (index-period-matched
  (range 15) P2mul
  (t/full-ts-gens 7) t/mul) t/mul false)

(println
 (call-embedding
  (range 15) P2mul
  TP2gens t/mul))

 (call-embedding
 (range 15) P2mul
 (range 15) P2mul)

(count (let [Tgens (t/full-ts-gens 4)]
         (embedding-backtrack
          Tgens t/mul
          (index-period-matched
           Tgens t/mul
           Tgens t/mul) t/mul true)))

(let [Tgens (t/full-ts-gens 2)]
  (index-period-matched
   Tgens t/mul
   Tgens t/mul))

(map count
     (index-period-matched
      (range 15) P2mul
      TP2gens t/mul))

(group-by (partial index-period P2mul) (range 15))

(group-by (partial index-period t/mul) (sgp-by-gens TP2gens t/mul))

(mtm/isomorphisms P2multab (mt/multab (sgp-by-gens TP2gens t/mul) t/mul))
