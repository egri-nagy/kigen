(ns kigen.semigroupoid.transformation
  "Transformation semigroupoids.
   :s - source, domain
   :t - target, codomain
   :m - morphism, map")

; example transformation semigroupoid
(def ExA1 ; example from semigroupoid paper
  {:objects [2,3]
   :gens [{:s 0, :t 0, :m [1 0]}
          {:s 1, :t 1, :m [1 2 0] }
          {:s 0, :t 1, :m [0 0]}
          {:s 1, :t 0, :m [0 0 0]}]})

(defn mul
  "Right multiplication of typed transformations."
  [a b]
  (when (= (:t a) (:s b))
    {:s (:s a)
     :t (:t b)
     :m (mapv (:m b) (:m a))}))

(defn sgpoid-by-gens
  [gens]
  (loop [S (set gens)]
    (print (count S) "\n")
    (let [generated (reduce
                     (fn [coll a]
                       (reduce into coll
                               [(map (partial mul a)
                                     (filter (comp (partial = (:t a)) :s) S))
                                (map #(mul % a)
                                     (filter (comp (partial = (:s a)) :t) S))]))
                     #{}
                     S)
          newelts (remove S generated)]
      (println (count newelts) newelts)
      (if (empty? newelts)
        S
        (recur (into S newelts))))))

(def ExA1full (sgpoid-by-gens (:gens ExA1)))


(filter #(and (= 0 (:t %)) (= 0 (:s %))) ExA1full)

(defn morphisms [S]
  (update-vals
   (group-by (fn [a] [(:s a) (:t a)]) S)
   (partial map :m)))

(morphisms ExA1full)