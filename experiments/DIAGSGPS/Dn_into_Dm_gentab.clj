;; counting embeddings of full diagram semigroups into other full diagram
;; semigroups of the same type
;; generic code for diagram semigroups using the transformation representation
(require '[kigen.transf :as t]
         '[kigen.sgp :refer [->Sgp]]
         '[kigen.conjugacy :as c]
         '[kigen.transf-conj :as t-c]
         '[kigen.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/merge-config! {:level :info})

(defn Dn-into-Dm [gf n m]
  (let [Dngens (gf n)
        Dmgens (gf m)
        targets (gmorph/index-period-matched (->Sgp Dngens t/mul)
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
  (def result (Dn-into-Dm t/full-ts-gens 3 6))
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

(println (count result))

(pprint (filter (fn [m]
                  (let [ invd (into #{}
                                    (map involved (vals m)))]
                    (= (count (first (vals m)))
                       (count invd))))
                result))

