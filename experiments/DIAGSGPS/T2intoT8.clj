;; embedding T2 into T8
;; number of ways: 879 (first computed 2017.03.22)

(def T8conjreps (clojure.edn/read-string
                 (slurp "experiments/DIAGSGPS/T8conjreps")))

(def S8 (t/sgp-by-gens (t/symmetric-gens 8)))

(def index-period-lookup
  (group-by #(sgp/index-period % t/mul) T8conjreps))

(def targets [(index-period-lookup [1 2])
              (mapcat (fn [cr] (distinct (map #(t/conjugate cr %) S8)))
                      (index-period-lookup [1 1]))])

(println (map count targets))

(println
 (count
  (gmorph/embeddings-conj [[1 0] [0 0]] t/mul targets t/mul t/conjugate S8)))
