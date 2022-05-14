;; Subsemigroup enumeration by minimal generating sets.

(require '[kigen.transf :as t])
(require '[kigen.transf-conj :as t-c])
(require '[kigen.multab :as mt])
(require '[kigen.conjugacy :as conjugacy])

(require '[orbit.core :as orb])

(require '[clojure.data.int-map :as i-m])
(require '[clojure.set :refer [map-invert]])

(defn db-filter
  "Checking the subs against the database, returning the ones that are not in the
  database yet.
  subs is  a map subsgp -> genset
  db is a map of integers (size of subsemigroup) to a map
  of subsemigroups to their generator sets."
  [db subs]
  (remove (fn [[sgp _]]
            ;;using short-circuit eval to avoid checking when size is new
            (and (db (count sgp)) ;do we know about this size?
                 ((db (count sgp)) sgp))) ;if yes, do we have the sgp?
          subs))

(defn extend-db
  "Adding sub-genset pairs to the database."
  [db subs]
  (reduce (fn [db [sgp gens]]
            (let [n (count sgp)
                  c (or (db n) ;do we have it? if yes, give the map
                        {})] ;otherwise start a new sgp->gens map
              (assoc db n (assoc c sgp gens))))
          db
          subs))

(defn extend-sub
  "Takes a subsgp-genset pair and finds all the distinct subsemigroups obtained
  by throwing in one new element."
  [[subS gens] mtS]
  ;;(println subS "-" gens "hey!" )
  (reduce ;over the missing elements
   (fn [m e]
     (conj m ;this takes care of duplicates
           [(mt/closure mtS
                        (into subS [e]))
            (conj gens e)]))
   {} ; a map from subsgp to generating set
   (i-m/difference (mt/elts mtS) subS)))

(defn layer
  "takes a queue a database, and returns an updated db and the newly discovered sgps"
  [q db mtS]
  (loop [q q db db next-layer {}]
    (let [exts (extend-sub (first q) mtS)
          news (db-filter db exts)
          newdb (extend-db db news)
          nn-l (into next-layer news)]
      (if (empty? (rest q))
        [newdb nn-l]
        (recur (rest q) newdb nn-l)))))

(defn subsgps
  [S]
  (let [vS (vec (sort S))
        mtS (mt/multab vS t/mul)
        elts (mt/elts mtS)]
    (loop [q { (i-m/int-set) (i-m/int-set)}
           db {}]
      (let [[ndb nq] (layer q db mtS)]
        (println "total: " (count ndb) "new: " (count nq))
        (if (empty? nq)
          ndb
          (recur nq ndb))))))

;(def S3 (t/sgp-by-gens (t/symmetric-gens 3)))
(def T3 (t/sgp-by-gens (t/full-ts-gens 3)))
;;(clojure.pprint/pprint (subsgps T3))
(clojure.pprint/pprint  (let [result (subsgps T3)]
                          (for [k (sort (keys result))]
                            [k (count (result k))])))


;(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))

;(def K42 (t/sgp-by-gens [ [ 0, 1, 1, 1 ], [ 0, 0, 2, 2 ], [ 2, 0, 0, 2 ], [ 3, 1, 3, 3 ], [ 2, 2, 0, 2 ], [ 0, 3, 0, 3 ], [ 2, 2, 3, 2 ], [ 1, 1, 2, 2 ], [ 2, 2, 2, 0 ] ]))


;(time (def T3subs (subsgps-up-to-conjugacy T3)))
;(time (def T3subs2 (subsgps-up-to-conjugacy2 T3 S3)))
;(println (count T3subs) " vs " (count T3subs2))

;(time (def K42subs2 (subsgps-up-to-conjugacy2 K42 S4)))
