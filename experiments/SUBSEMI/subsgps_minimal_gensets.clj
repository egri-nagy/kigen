;; Subsemigroup enumeration by minimal generating sets.

(require '[kigen.transf :as t])
(require '[kigen.transf-conj :as t-c])
(require '[kigen.multab :as mt])
(require '[clojure.data.int-map :as i-m])
(require '[progrock.core :as pr])

(defn extend-db
  "Adding sub-genset pairs to the database.
  Also checking the subs against the database, returning the ones that are not in the
  database yet.
  subs is  a map subsgp -> genset
  db is a map of integers (size of subsemigroup) to a map
  of subsemigroups to their generator sets."
  [db subs]
  (reduce ;over the subs
   (fn [[db news :as dbnews] [sgp gens :as sgpgens]]
     (let [n (count sgp)
           c (or (db n) ;do we have it? if yes, give the map
                 {})] ;otherwise start a new sgp->gens map
       (if (contains? c sgp)
         dbnews
         [(assoc db n (assoc c sgp gens)) (conj news sgpgens)])))
   [db []] ;we start with the db and empty vector for the new subs
   subs))

(defn write-db
  "Writes the database into a file flattening into a sequence of entries.
  Loading requires to use extend-db line by line, but it is possible convert
  to dense-int-sets."
  [db filename]
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [c (keys db)
            entry (seq (db c))]
      (.write w (prn-str entry)))))

(defn write-map
  "Writes a map into a file one entry per line."
  [m filename]
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [c (seq m)]
      (.write w (prn-str c)))))

(defn extend-sub
  "Takes a subsgp-genset pair and finds all the distinct subsemigroups obtained
  by throwing in one new element."
  [[subS gens] mtS crf]
  (reduce ;over the missing elements
   (fn [m e]
     (conj m ;this map takes care of duplicates (may not record the first hit)
           [(crf (mt/closure mtS subS (i-m/dense-int-set [e]))) ;we compute conjrep
            (conj gens e)])) ;the gens may not generate the conjrep
   {} ;a map from subsgp to generating set
   (i-m/difference (mt/elts-cached mtS) subS)))

(defn layer
  "takes a queue a database, and returns an updated db and the newly discovered sgps
  depending on the suplied map function (map or pmap) computation can be single
  or multi core, it provides a progress bar as well"
  [q db mtS crf mapfn]
  (let [bar (pr/tick (pr/progress-bar (count q)) 1)]
    (reduce
     (fn [[db next-layer bar] exts]
       (let [[newdb news] (extend-db db exts)
             nbar (pr/tick bar 1)]
         (pr/print bar
                   {:length 50 :format ":percent% :progress/:total |:bar| ETA::remaining :elapsed"})
         [newdb
          (into next-layer news)
          (if (= (:progress nbar) (:total nbar))
            (pr/done nbar)
            nbar)]))
     [db {} bar] ;we build the database, the next-layer and the progress bar
     (mapfn #(extend-sub % mtS crf) q))))

(defn subsgps
  ;;starting with an empty database, the empty set to build the first layer
  ([S G mapfn]
   (subsgps S G mapfn {(i-m/dense-int-set) (i-m/dense-int-set)} {} 1 0))
  ;;this can be used for restarting computations
  ([S G mapfn q db n total]
   (let [mtS (mt/multab (vec (sort S)) t/mul)
         crf (t-c/setconjrepfunc S G )]
     (loop [q q db db n n t total]
       (let [[ndb nq] (layer q db mtS crf mapfn)
             card (db n)
             nndb (if card
                    (dissoc ndb n) ;shedding the cardinality we will not check any more
                    ndb)]
         (write-db nndb (str "db" (format "%03d" n)))
         (spit (str "layer" (format "%03d" n)) (prn-str nq))
         (when card (write-map card (str "cardinality" (format "%03d" n))))
         (println (str n"-generated:") (count nq) "total:" t)
         (if (empty? nq)
           (doseq [k (keys nndb)] ;we do not return anything, everything is in files
             (write-map (nndb k)  (str "cardinality" (format "%03d" k))))
           (recur nq
                  nndb
                  (inc n)
                  (+ t (count nq)))))))))

(def T4 (t/sgp-by-gens (t/full-ts-gens 4)))
(def S4 (t/sgp-by-gens (t/symmetric-gens 4)))

;(load-file "K42.clj")

(time (subsgps (t/sgp-by-gens T4) S4 pmap))

