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
(require '[clojure.pprint :refer [pprint]])

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
;(map #(count (into #{} (map conjrep (selections (range %) %)))) (range 1 8))


(defn post-arrow
  "Lookup for the single arrow post-composing with a give arrow in a functional
   digraph."
  [t]
  (let [pts (range (count t))
        point2arrow (zipmap pts (t/single-maps t))]
    (reduce
     (fn [m arrow]
       (conj m [arrow (point2arrow (second arrow))]))
     {}
     (vals point2arrow))))

(defn pre-arrow
  "Lookup for the set of arrows pre-composing with a give arrow in a functional
   digraph."
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
  "Trying to match the mapping from a `t-arrow` to a 'r-arrow' consistent with
   the existent mapping `m`."
  [post-arrow pre-arrow t-arrow r-arrow m]
  (println "Matching " t-arrow "with" r-arrow)
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

;compress
(defn compress
  [psols]
  (let [base (first psols)
        basepoint (:last base)
        imgs (mapv :last (rest psols))]
    (-> base
        (conj base [:gens {basepoint imgs}])
        (dissoc :last))))

;decompressing
(defn decompress
  ([compressed] ; no baspoint given, we do all
   (let [basepoints (keys (:gens compressed))]
     (mapcat
      (partial decompress compressed)
      basepoints)))
  ([{sources :sources p :p gens :gens} basepoint]
  ;(println sources p (keys gens))  
   (into [{:sources sources :p p}] ;the one kept in the compressed format 
         (map (fn [img]
                (let [hm {basepoint img, img basepoint}
                      hm+id (fn [arr] (hm arr arr))]
                  {:sources (set (map  hm+id sources))
                   :p (update-vals p hm+id)}))
              (gens basepoint)))))

(defn match-all-sources
  [post-arrow pre-arrow {sources :sources m :p :as psol} target]
  (remove nil?
          (map
           (fn [src]
             (let [nm (match post-arrow pre-arrow src target m)]
               (println "nm" nm)
               (when nm
                 (-> psol
                     (conj [:p nm])
                     (conj [:sources (disj sources src)])
                     (conj [:last src])))))
           sources)))

(defn match-all
  "j"
  [post-arrow pre-arrow psols target]
  ;(println "target" target)
  (mapcat (fn [psol]
            (match-all-sources post-arrow pre-arrow psol target))
          psols))

(defn match-compressed
  "takes a compressed partial solution and retruns compressed extended
   solutions"
  [post-arrow pre-arrow
   {gens :gens :as compressed-psol}
   target]
  (let [basepoints (keys gens)]
    (mapcat
     (fn [basepoint]
       (println "basepoint" basepoint)
       (let [all (decompress compressed-psol basepoint)]
         (println "decompressed" all)
         (mapcat (fn [{sources :sources m :p :as psol}]
                               ;(println "sources" sources "m" m)
                   (remove nil?
                           ))
                 all)))
     basepoints)))


(defn kickstart
  [t target]
  (match-all (post-arrow t)
             (pre-arrow t)
             [{:sources (set (t/single-maps t))
               :p {}
               :gens {}}]
             target))

;; do one in detail
(def t [0 1 0])
(def compressed (compress (kickstart [0 1 0] [0 0])))
(def r (match-compressed (post-arrow t) (pre-arrow t) compressed [1 0]))
(match-all (post-arrow t) (pre-arrow t) [ {:sources #{[1 1]}, :p {[0 0] [0 0], [2 0] [1 0]}, :last [2 0]}] [2 2])


(def t [3 3 3 3])
(def compressed (compress (kickstart t [0 0])))
(match-compressed (post-arrow t) (pre-arrow t) compressed [1 0])


;compressed form
(def compressed {:sources #{[1 1] [2 0]}
                 :p {[0 0] [0 0]}
                 :gens {[0 0] [[1 1]]}})

(decompress compressed)


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
                             [0 [[sources  {}]] [0 pt]]))
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

(conjrep [0 1 0])

;checking against the library
(filter
 #(not (= (t-c/conjrep %)
          (conjrep %)))
 (selections (range 3) 3))