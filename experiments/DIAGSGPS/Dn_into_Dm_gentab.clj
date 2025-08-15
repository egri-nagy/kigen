;; counting embeddings of full diagram semigroups into other full diagram
;; semigroups of the same type
;; generic code for diagram semigroups using the transformation representation
(require '[kigen.diagram.transf :as t]
         '[kigen.semigroup.sgp :refer [->Sgp]]
         '[kigen.semigroup.conjugacy :as c]
         '[kigen.diagram.transf-conj :as t-c]
         '[kigen.semigroup.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/set-min-level! :trace)

(defn Dn-into-Dm [gf n m]
  (let [Dngens (gf n)
        Dmgens (gf m)
        targets (gmorph/index-period-matched Dngens t/mul
                                             Dmgens t/mul)]
    (gmorph/embeddings-distinct
     Dngens
     t/mul
     targets
     t/mul
     (c/->ConjugationFunctionBundle t-c/conjrep
                                    t-c/setconjrep
                                    t-c/conj-conj))))

(defn Dn->Dm-table [gf n]
  (let [pairs (for [i (range 1 (inc n))
                    j (range 1 (inc i))]
                [j i])]
    (map (fn [[m n]]
           (let [r (count (Dn-into-Dm gf m n))]
             (println \# m n r)
             [[m n] r]))
         pairs)))

;(binding [orbit.extension/*task-size* 1]
  (def result (Dn-into-Dm t/full-ts-gens 3 4))
 ; )

(defn involved
  "all the points that are involved in the transformation"
  [t] 
  (reduce (fn [result [src img]]
            (if-not (= src img)
              (conj result img)
              result))
          #{}
          (map vector (range) t)))
  
(defn all-involved
  [ts]
  (reduce into #{} (map involved ts)))  

(println (count result))
  
(def ir (filter (fn [m]
                  (= (count (first (vals m)))
                     (count (all-involved (vals m)))))
                result))
  
(println (count ir) )

(defn print-gens
  [solution]
  (doseq [gen (t/full-ts-gens 3)]
    (println gen "->" (solution gen))))

(map print-gens ir)