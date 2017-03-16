(defn full-ts-conjreps [n]
  (let [Tn (transf/sgp-by-gens (transf/full-ts-gens n))
        Sn (transf/sgp-by-gens (transf/symmetric-gens n))
        ccls (group-by
              (fn [t] (kigen.conjugacy/conjrep transf/conjugate t Sn))
              Tn)]
    (map first ccls)))
