;; embedding T3 into T8
;; computed 736 distinct embeddings 2017.03.23

(def T8conjreps (clojure.edn/read-string
                 (slurp "experiments/DIAGSGPS/T8conjreps")))

(def S8 (t/sgp-by-gens (t/symmetric-gens 8)))

(def index-period-lookup
  (group-by #(sgp/index-period % t/mul) T8conjreps))

(def T3gens (t/full-ts-gens 3))
(def T3ips (map #(sgp/index-period % t/mul) T3gens))

(def targets (concat  [(index-period-lookup (first T3ips))]
                      (map
                       (fn [gen] (mapcat (fn [cr] (distinct (map #(t/conjugate cr %) S8)))
                                         (index-period-lookup gen)))
                       (rest T3ips))))

(println (map count targets))

(println
  (count
   (gmorph/embeddings-conj T3gens t/mul targets t/mul t/conjugate S8)))
