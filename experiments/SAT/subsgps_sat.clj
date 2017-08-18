(require '[rolling-stones.core :as sat :refer [! at-least at-most exactly]])

(defn multabCNF [mt]
  (let [n (count mt)
        elts (range n)]
    (for [i elts j elts]
      [(- (inc i)) (- (inc j)) (inc (multab/at mt i j))])))

;;(map (partial filter pos?) (sat/solutions mt))

;; this slows down the SAT-solver
(defn gentabCNF [gens]
  (let [gt (gentab/gentab gens t/mul)
        m (count (:gens gt))
        n (count (:tab gt))
        tab (:tab gt)]
    (for [i (range n) j (range m)]
      [(- (inc i)) (- (inc j)) (inc (multab/at  tab i j))])))

(defn mtdat [gens G]
  (let [S (t/sgp-by-gens gens)
        vS (vec (sort S))
        mtvS (multab/multab vS t/mul)
        n (count vS)
        indices (vec (range n))
        t2i (clojure.set/map-invert (zipmap (range n) vS))
        ;; mapping indices to the index of the conjugacy rep
        conjreps (zipmap indices
                         (map (fn [x] (t2i (t/conjrep (vS x))))
                              indices))
        cjr2cjcl (group-by conjreps indices)
        r2l (into {} (map (fn [[k vs]] [(inc k) (mapv inc vs)]) cjr2cjcl))
        orderedks (vec (sort (keys r2l)))
        Ghom (fn [p] (mapv t2i (map #(t/conjugate % p) vS)))
        H (map Ghom G)]
    {:sgp S
     :sgp_ordered vS
     :multab mtvS
     :indices indices
     :transf2index t2i
     :conjreps conjreps
     :conjrep2conjclass cjr2cjcl
     :LOGICconjrep2conjclass r2l
     :syms H}))

(defn partition-transformation [t]
  (let [m (group-by (fn [i] (compare (t i) i))
                    (range (count t)))]
    {:dec (m -1)
     :fix (m 0)
     :inc (m 1)}))

(defn conjCNF [gens G]
  (let [Sdat (mtdat gens G)]
    (multabCNF (:multab Sdat))))

(defn spit-solutions
  "Given a conjunctive normal form, using the sat-solver it lazily
  goes thorugh the solutions and spits them into the given file."
  [cnf file]
  (map #(spit file
              (str (vec(remove neg? %)) \newline)
              :append true)
       (sat/solutions cnf)))

(defn clause-conjreps [Sdat]
  (keys (:LOGICconjrep2conjclass Sdat)))

(defn clauses-conjclasses [Sdat]
  (let [r (apply min
                 (keys (:LOGICconjrep2conjclass Sdat)))
        elts (remove #{r} ((:LOGICconjrep2conjclass Sdat) r))]
    (for [x elts] [(- x) r])))


(defn spit-subsgps [gens file]
  (let [S (t/sgp-by-gens gens)
        vS (vec (sort S))
        mtvS (multab/multab vS t/mul)]
    (spit-solutions (multabCNF mtvS) file)))

(defn cnfDIMACS
  [nliterals cnfs filename]
  (with-open [w (clojure.java.io/writer  filename :append true)]
    (.write w (str "p cnf " nliterals " " (count cnfs) \newline))
    (doseq [v cnfs]
      (.write w (str (clojure.string/join \space v)
                     " 0" \newline)))))

(def T4 (t/sgp-by-gens (t/full-ts-gens 4)))
(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))

(def T3 (t/sgp-by-gens (t/full-ts-gens 3)))
(def S3 (t/sgp-by-gens (t/symmetric-gens 3)))
