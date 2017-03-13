(defn Tm->Tn [m n]
  (let [Tmgens (transf/full-ts-gens m)
        result (gmorph/sgp-embeddings-by-gens Tmgens transf/mul
                                              (transf/full-ts-gens n) transf/mul
                                              (transf/sgp-by-gens
                                               (transf/symmetric-gens n)))]
    result))

(defn Tm->Tn-table []
  (let [pairs (for [i (range 1 (inc 6)) j (range 1 (inc i)) ] [j i])]
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
