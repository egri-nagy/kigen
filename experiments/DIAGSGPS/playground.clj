(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as t])
(require '[kigen.diagram.transf-conj :as t-c])
(require '[kigen.semigroup.conjugacy :as c])
(require '[kigen.semigroup.genmorph :refer [sgp-embeddings-by-gens]])
(require '[kigen.canonical-labeling :refer [can-set-seq can-seq]])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[taoensso.timbre :as timbre])
(require '[clojure.set :refer [union]])

(timbre/set-min-level! :trace)

(def S5 (sgp-by-gens (t/symmetric-gens 5) t/mul))
;; (def T5 (sgp-by-gens (t/full-ts-gens 5) t/mul))
;; (def S4 (sgp-by-gens (t/symmetric-gens 4) t/mul))
;; (def T4 (sgp-by-gens (t/full-ts-gens 4) t/mul))
(def S2 (sgp-by-gens (t/symmetric-gens 2) t/mul))
(def S3 (sgp-by-gens (t/symmetric-gens 3) t/mul))
(def T3 (sgp-by-gens (t/full-ts-gens 3) t/mul))
;; (def S6 (sgp-by-gens (t/symmetric-gens 6) t/mul))
;; (def T6 (sgp-by-gens (t/full-ts-gens 6) t/mul))

;; checking number of conjreps
(map
 #(count (into #{} (map conjrep (selections (range %) %))))
 (range 1 8))


(defn post-arrow
  [t]
  (let [pts (range (count t))
        point2arrow (zipmap pts (t/single-maps t))]
    (reduce
     (fn [m arrow]
       (conj m [arrow (point2arrow (second arrow))]))
     {}
     (vals point2arrow))))

(defn pre-arrow
  [t]
  (let [maps (t/single-maps t)
        image2arrow (group-by second maps)]
    (reduce
     (fn [m arrow]
       (conj m [arrow (set (image2arrow (first arrow)))]))
     {}
     maps)))

(post-arrow [2 0 0 3])
(pre-arrow [2 0 0 3])
(t/single-maps [2 0 0 3])
(pre-arrow [0 0 0 0 0])

(pre-arrow {0 0})
(pre-arrow {4 4}) ; not working


(defn match
  [post-arrow pre-arrow t-arrow r-arrow m]
  (let [mm (conj m [t-arrow r-arrow])]
    (when (and
           ;post composition easier to check
           (let [post-in-r (mm (post-arrow t-arrow));source's single post arrow mapped to r
                 arr (first (filter (fn [arr] (= (first arr) (second r-arrow)))
                                    (vals mm)))]
             ;(println post-in-r arr)
             (or (and (nil? post-in-r) ;not known yet
                      (nil? arr))
                 (= arr post-in-r)))
           (let [pre-in-rs (set (filter identity (map mm (pre-arrow t-arrow))));source's single post arrow mapped to r
                 arrs (filter (fn [arr] (= (second arr) (first r-arrow)))
                              (vals mm))]
             ;(println pre-in-rs arrs)
             (or (and (empty? pre-in-rs)
                      (empty? arrs))
                 (not (empty? (filter pre-in-rs arrs))))))
      mm)))

(def t [2 0 0 3])
(match (post-arrow t)
  (pre-arrow t)
  [3 3] [1 0] {})

(def t2 [1 2 3 0])
(match (post-arrow t2)
  (pre-arrow t2)
  [1 2] [3 0] {[0 1] [1 2]})

(defn matchall
  "j"
  [post-arrow pre-arrow psols target]
  ;(println "target" target)
  (remove nil? (mapcat (fn [[sources m]]
                      ;(println "sources" sources "m" m)
                      (map
                       (fn [src]
                         ;(println "src" src)
                         (let [nm (match post-arrow pre-arrow src target m)]
                           (when nm
                             [(disj sources src) nm])))
                       sources))
                    psols)))

(def t3 [0 1 0])
(def r (matchall (post-arrow t3)
                 (pre-arrow t3)
                 [[(set (t/single-maps t3)) {}]]
                 [0 0]))

r

(defn conjugators
  "Direct construction of conjugacy class representative of transformation t."
  [t]
  (let [n (count t)
        pts (reverse (range n)) ;to make sure we start with zero (we use stack) so we get minimum
        sources (set (t/single-maps t))
        posts (post-arrow t)
        pres (pre-arrow t)
        matchallfn (partial matchall posts pres)
        initial_stack (vec (for [pt pts]
                             [0 [[sources  {} ]] [0 pt]]))
        search (fn [stack]
                 ;(println "stack!" (apply str (map (fn [[k psols tg]] (str k " psols" psols " tg")) (reverse stack))))
                 (let [[k psols target] (peek stack)
                       npsols (matchallfn psols target)]
                   ;(println "new" npsols)
                   ;(println "k" k)
                   (if (empty? npsols)
                     (recur (pop stack))
                     (if (= k (dec n))
                       npsols
                       (recur (into (pop stack)
                                    (for [pt pts]
                                      [(inc k)
                                       npsols 
                                       [(inc k) pt]])))))))]
    ;(println "initial stack" initial_stack)
    (search initial_stack)))

(defn conjrep
  [t]
  (mapv second (sort  (vals (second (first (conjugators t)))))))

(count (conjrep [1 1 1 1 1 1 1 1 1 1]))

(def s [0 1 0]) (t/single-maps s)
(mapv second (sort  ( vals (second (first (conjrep s))))))
(conjrep s)
(t-c/conjrep s)


(filter
 #(not (= (t-c/conjrep %)
          (mapv second (sort (vals (conjrep %))))))
 (selections (range 3) 3))