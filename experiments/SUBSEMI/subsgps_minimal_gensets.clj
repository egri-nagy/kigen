;; Subsemigroup enumeration by minimal generating sets.

(require '[kigen.transf :as t])
(require '[kigen.transf-conj :as t-c])
(require '[kigen.multab :as mt])
(require '[clojure.data.int-map :as i-m])

(defn extend-db
  "Adding sub-genset pairs to the database.
  Also checking the subs against the database, returning the ones that are not in the
  database yet.
  subs is  a map subsgp -> genset
  db is a map of integers (size of subsemigroup) to a map
  of subsemigroups to their generator sets."
  [db subs]
  (reduce (fn [[db news :as dbnews] [sgp gens :as sgpgens]]
            (let [n (count sgp)
                  c (or (db n) ;do we have it? if yes, give the map
                        {})] ;otherwise start a new sgp->gens map
              (if (contains? c sgp)
                dbnews
                [(assoc db n (assoc c sgp gens)) (conj news sgpgens)])))
          [db []]
          subs))

(defn extend-sub
  "Takes a subsgp-genset pair and finds all the distinct subsemigroups obtained
  by throwing in one new element."
  [[subS gens] mtS crf]
  (reduce ;over the missing elements - not big enough for parallel for T4
   (fn [m e]
     (conj m ;this map takes care of duplicates (may not record the first hit)
           [(crf (mt/closure mtS subS (i-m/dense-int-set [e]))) ;we computer conjrep
            (conj gens e)])) ;the gens may not generate the conjrep
   {} ;a map from subsgp to generating set
   (i-m/difference (mt/elts mtS) subS)))

(defn layer
  "takes a queue a database, and returns an updated db and the newly discovered sgps
  depending on the suplied map function (map or pmap) computation can be single
  or multi core"
  [q db mtS crf mapfn]
  (reduce
   (fn [[db next-layer] exts]
     (let [[newdb news] (extend-db db exts)]
       [newdb (into next-layer news)]))
   [db {}]
   (mapfn #(extend-sub % mtS crf) q)))

(defn subsgps
  ;;starting with an empty database, the empty set to build the first layer
  ([S G mapfn]
   (subsgps S G mapfn {(i-m/dense-int-set) (i-m/dense-int-set)} {} 1))
  ;;this can be used for restarting computations
  ([S G mapfn q db n]
   (let [mtS (mt/multab (vec (sort S)) t/mul)
         crf (t-c/setconjrepfunc S G )]
     (loop [q q db db n n]
       (let [[ndb nq] (layer q db mtS crf mapfn)]
         (spit (str "db" n) (prn-str ndb))
         (spit (str "gens" n) (prn-str nq))
         (println (str n"-generated:") (count nq) "total:" (apply + (map count (vals ndb))))
         (if (empty? nq)
           ndb
           (recur nq
                  ndb
                  (inc n))))))))

(defn convert
  "map sets to sets -> int-sets to int-sets"
  [m]
  (into {} (map (fn [[k v]]
                  [(i-m/dense-int-set k) (i-m/dense-int-set v)])
                m)))

(defn print-result [S G q]
  (clojure.pprint/pprint  (let [result (subsgps S G pmap)]
                                                ;(read-string (slurp q)) (read-string (slurp db)) n)]
                            (for [k (sort (keys result))]
                              [k (count (result k))]))) )

(def T4 (t/sgp-by-gens (t/full-ts-gens 4)))
(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))

(load-file "K42.clj")

(time (print-result (t/sgp-by-gens K42) S4 pmap)) ;"gens2" "db2" 2))

