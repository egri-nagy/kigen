;; embedding T2 into T8
;; number of ways: 879 (first computed 2017.03.22)
;; embedding T3 into T8
;; computed 736 distinct embeddings 2017.03.23 for T3 -> T8
;; 48 for T4->T8

;; precalculated
(def T8conjreps (clojure.edn/read-string
                 (slurp "experiments/DIAGSGPS/T8conjreps")))

(def S8 (t/sgp-by-gens (t/symmetric-gens 8)))

(defn t-i-p [x] (sgp/index-period x t/mul))

(def index-period-lookup
  (group-by t-i-p T8conjreps))

(def n 7)

(def Tngens (t/full-ts-gens n))
(def Tnips (map t-i-p Tngens))

(def targets (concat  [(index-period-lookup (first Tnips))]
                      (map
                       (fn [gen] (mapcat (fn [cr] (distinct
                                                   (map #(t/conjugate cr %)
                                                        S8)))
                                         (index-period-lookup gen)))
                       (rest Tnips))))

(println (map count targets))

(println
  (count
   (gmorph/embeddings-conj Tngens t/mul targets t/mul
                           t/conjrep t/conj-conj t/setconjrep)))
