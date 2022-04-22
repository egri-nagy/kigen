;; Subsemigroup enumeration by minimal generating sets.

(require '[kigen.transf :as t])
(require '[kigen.transf-conj :as t-c])
(require '[kigen.multab :as mt])
(require '[kigen.conjugacy :as conjugacy])

(require '[orbit.core :as orb])

(require '[clojure.data.int-map :as i-m])
(require '[clojure.set :refer [map-invert]])

(defn gen1
  [S]
  (let [vS (vec (sort S))
        mtS (mt/multab vS t/mul)
        elts (mt/elts mtS)
        extend (fn [[subS gens]]
                 (reduce
                  (fn [m e]
                    (conj m
                          [(mt/closure mtS
                                       (into (i-m/int-set) [e]))
                           (conj gens e)]))
                  {}
                  (i-m/difference elts subS)))]
    (extend [(i-m/int-set) (i-m/int-set)])))

;(def S3 (t/sgp-by-gens (t/symmetric-gens 3)))
(def T3 (t/sgp-by-gens (t/full-ts-gens 3)))
(println (gen1 T3))


;(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))

;(def K42 (t/sgp-by-gens [ [ 0, 1, 1, 1 ], [ 0, 0, 2, 2 ], [ 2, 0, 0, 2 ], [ 3, 1, 3, 3 ], [ 2, 2, 0, 2 ], [ 0, 3, 0, 3 ], [ 2, 2, 3, 2 ], [ 1, 1, 2, 2 ], [ 2, 2, 2, 0 ] ]))


;(time (def T3subs (subsgps-up-to-conjugacy T3)))
;(time (def T3subs2 (subsgps-up-to-conjugacy2 T3 S3)))
;(println (count T3subs) " vs " (count T3subs2))

;(time (def K42subs2 (subsgps-up-to-conjugacy2 K42 S4)))
