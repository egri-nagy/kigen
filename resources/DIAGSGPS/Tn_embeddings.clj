(defn Tm->Tn [m n]
  (let [Tmgens (t/full-ts-gens m)
        result (gmorph/sgp-embeddings-by-gens Tmgens t/mul
                                              (t/full-ts-gens n) t/mul
                                              t/conjugate
                                              (t/sgp-by-gens
                                               (t/symmetric-gens n)))]
    result))

(defn Tm->Tn-table []
  (let [pairs (for [i (range 1 (inc 6)) j (range 1 i) ] [j i])]
    (map (fn [[m n]] [m n (count (Tm->Tn m n))]) pairs)))

;;just print this
(defn Tn->Tn+1 [maxdeg]
  (letfn  [(incvec [v] (mapv inc v))]
    (apply str
           (mapcat
            #(for [ms (Tm->Tn % (inc %))]
               (str (apply str
                           (map (fn [m] (str (incvec (first m)) "->" (incvec (second m)) "\n"))
                                ms)) "\n") )
            (range 1 (inc maxdeg))))))
