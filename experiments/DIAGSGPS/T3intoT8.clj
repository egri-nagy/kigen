;; embedding T3 into T8

(def T8conjreps (clojure.edn/read-string
                 (slurp "experiments/DIAGSGPS/T8conjreps")))

(def S8 (t/sgp-by-gens (t/symmetric-gens 8)))

(def index-period-lookup
  (group-by #(sgp/index-period % t/mul) T8conjreps))

(def T3gens (full-ts-gens 3))

(def targets (concat  [(index-period-lookup (first T3gens))]
                      (map 
                       (fn [gen] (mapcat (fn [cr] (distinct (map #(t/conjugate cr %) S8)))
                                         (index-period-lookup gen)))
                       (rest T3gens))))

(println (map count targets))

;; (println
;;  (count
;;   (gmorph/embeddings-conj T3gens t/mul targets t/mul t/conjugate S8)))
