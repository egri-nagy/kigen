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

(timbre/set-min-level! :debug)

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

(post-arrow [1 1 1 1])
(pre-arrow [1 1 1 1])
(t/single-maps [1 1 1 1])

(post-arrow [2 0 0 3])
(pre-arrow [2 0 0 3])
(t/single-maps [2 0 0 3])

(pre-arrow [0 0 0 0 0])

;hash-maps work when they are fully defined
(pre-arrow [0])
(pre-arrow {0 0})
(post-arrow {0 0})
(post-arrow [0])

(defn match
  "Trying to match the mapping from a `t-arrow` to a 'r-arrow' consistent with
   the existent mapping `m`."
  [post-arrow pre-arrow t-arrow r-arrow m]
  ;(timbre/trace "Matching " t-arrow "with" r-arrow)
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

(defn testmatch
  [t source target m]
  (match (post-arrow t) (pre-arrow t) source target m))

;when it's there, just returns
(testmatch [0 0 0] [0 0] [2 2] {[0 0] [2 2]})
(testmatch [1 2 0] [0 1] [2 2] {}) ;should not work due to self-loop
;test a square, reversing the direction of the cycle
(testmatch [1 2 3 0]  [2 3] [2 1] {[0 1] [0 3]})
(testmatch [1 2 3 0] [1 2] [0 1] {[0 1] [0 3], [2 3] [2 1]}) ;[0 1] bad direction
(testmatch [1 2 3 0] [1 2] [1 0] {[0 1] [0 3], [2 3] [2 1]}) ;not conjugation?
(testmatch [1 2 3 0] [1 2] [3 2] {[0 1] [0 3], [2 3] [2 1]}) ;works!

;compress
(defn compress
  "Input: a coll of partial solutions with :last indicating last match
   Output: a collection of compressed ones"
  [psols]
  ;(timbre/trace "psols in compress" psols)
  (if (= 1 (count psols))
    (-> (first psols)
        (dissoc :last)
        (dissoc :gens))
    (let [base (first psols) ;the first one will be the base
          basepoint (:last base)
          imgs (mapv :last (rest psols))]
      (-> base
          (conj base [:gens {basepoint imgs}])
          (dissoc :last)))))

(compress [{:sources #{[1 1] [2 0]}, :p {[0 0] [0 0]}, :last [0 0]}
           {:sources #{[0 0] [2 0]}, :p {[1 1] [0 0]}, :last [1 1]}])

;decompressing
(defn decompress
  ([compressed] ; no basepoint given, we do all
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

(decompress {:sources #{[1 1] [2 0]}, :p {[0 0] [0 0]}, :gens {[0 0] [[1 1]]}})

(defn match-all-sources
  [post-arrow pre-arrow {sources :sources m :p :as psol} target]
  (let [results (remove nil?
                        (mapv
                         (fn [src]
                           (let [nm (match post-arrow pre-arrow src target m)]
                             ;(timbre/trace src target "nm" nm)
                             (when nm
                               (-> psol
                                   (conj [:p nm])
                                   (conj [:sources (disj sources src)])
                                   (conj [:last src])))))
                         sources))]
    ;(timbre/trace results)
    results))

(defn test-match-all-source
  [t psol target]
  (match-all-sources
   (post-arrow t)
   (pre-arrow t)
   psol
   target))

(test-match-all-source [0 1 0]
                       {:sources (set (t/single-maps [0 1 0])) :p {}}
                       [0 0])

(defn match-all
  "tba"
  [post-arrow pre-arrow psols target]
  (apply concat
         (remove empty?
                 (map
                  (fn [psol]
                    (compress (match-all-sources post-arrow pre-arrow psol target)))
                  psols))))

(defn match-compressed
  "takes a single compressed partial solution and returns a collection of
   extended compressed solutions, when no :gens present, no need to decompress"
  [post-arrow pre-arrow {gens :gens :as psol} target]
  ;(timbre/trace "Gens" gens)
  (if gens
    (let [basepoints (keys gens)]
      (mapcat
       (fn [basepoint]
         ;(timbre/trace "basepoint" basepoint)
         (let [all (decompress psol basepoint)]
           (timbre/trace "decompressed" all)
           (match-all post-arrow pre-arrow all target)))
       basepoints))
    (match-all-sources post-arrow pre-arrow psol target)))


(defn kickstart
  [t target]
  (match-all-sources (post-arrow t)
                     (pre-arrow t)
                     {:sources (set (t/single-maps t))
                      :p {}}
                     target))

;; do one in detail
(def t [0 1 0])
(def compressed (kickstart [0 1 0] [0 0]))
(def r (match-compressed (post-arrow t) (pre-arrow t) compressed [1 0]))
(match-compressed (post-arrow t) (pre-arrow t)
                  {:sources #{[1 1]}, :p {[0 0] [0 0], [2 0] [1 0]}} [2 2])



(def t [3 3 3 3])
(def compressed (compress (kickstart t [1 0])))
(match-compressed (post-arrow t) (pre-arrow t) (first compressed) [1 0])


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
        match-compressed-fn (partial match-compressed posts pres)
        initial_stack (vec (for [pt pts]
                             [0
                              [{:sources sources :p {}}]
                              [0 pt]]))
        search (fn [stack]
                 ;(timbre/trace "stack!"  (reverse stack) "\n")
                 (timbre/trace "PEEK STACK" (peek stack))
                 (let [[k psols target] (peek stack)
                       npsols (mapcat
                               (fn [psol]
                                 (match-compressed-fn psol target))
                               psols)]
                   (timbre/trace "NPSOLS" npsols)
                   (if (empty? npsols)
                     (recur (pop stack))
                     (if (= k (dec n))
                       npsols
                       (recur (into (pop stack)
                                    (for [pt pts]
                                      [(inc k)
                                       npsols
                                       [(inc k) pt]])))))))]
    (search initial_stack)))

(conjugators [0 1 0])
(conjugators [1 1])

(conjugators [1 1 1 2])
(conjugators [2 2 2 2 2 2])
(t-c/conjrep [1 1 1 2])

(def m-c-fn (partial match-compressed
                     (post-arrow [0 1 0])
                     (pre-arrow [0 1 0])))

(m-c-fn {:sources #{[1 1] [2 0]}, :p {[0 0] [0 0]}, :gens {[0 0] [[1 1]]}} [1 0])

(m-c-fn {:sources #{[1 1]}, :p {[0 0] [0 0], [2 0] [1 0]}} [2 2])


;;doing [1 1] manually
(def m-c-fn (partial match-all
                     (post-arrow [1 1])
                     (pre-arrow [1 1])))
(kickstart [1 1] [0 0])
(m-c-fn {:sources #{[0 1]}, :p {[1 1] [0 0]}} [1 0])


(kickstart [0 1 0] [0 0])
(kickstart [1 1] [0 0])

(defn conjrep
  [t]
  (mapv second (sort  (vals (:p (first (conjugators t)))))))

(conjrep [1 2 3 4 0])
(t-c/conjrep [1 2 3 4 0])



;checking against the library
(filter
 #(not (= (t-c/conjrep %)
          (conjrep %)))
 (selections (range 6) 6))
