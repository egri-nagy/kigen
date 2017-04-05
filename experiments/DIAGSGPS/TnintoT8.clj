;; embedding T3 into T8
;; computed 736 distinct embeddings 2017.03.23 for T3 -> T8
;; 48 for T4->T8

(def T8conjreps (clojure.edn/read-string
                 (slurp "experiments/DIAGSGPS/T8conjreps")))

(def S8 (t/sgp-by-gens (t/symmetric-gens 8)))

(def index-period-lookup
  (group-by #(sgp/index-period % t/mul) T8conjreps))

(def n 7)

(def Tngens (t/full-ts-gens n))
(def Tnips (map #(sgp/index-period % t/mul) Tngens))

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
