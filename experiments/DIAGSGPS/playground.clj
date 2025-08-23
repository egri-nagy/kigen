(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as t])
(require '[kigen.diagram.pbr :as pbr])
(require '[kigen.diagram.transf-conj :as t-c])
(require '[kigen.semigroup.conjugacy :as c])
(require '[kigen.semigroup.genmorph :refer [sgp-embeddings-by-gens class-reps]])
(require '[kigen.canonical-labeling :refer [can-set-seq can-seq]])
(require '[kigen.table.multab :as mt])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[taoensso.timbre :refer [trace set-min-level!]])
(require '[clojure.set :refer [union]])
(require '[clojure.pprint :refer [pprint]])

(set-min-level! :trace)
(set-min-level! :debug)


(def S5 (sgp-by-gens (t/symmetric-gens 5) t/mul))
;; (def T5 (sgp-by-gens (t/full-ts-gens 5) t/mul))
;; (def S4 (sgp-by-gens (t/symmetric-gens 4) t/mul))
;; (def T4 (sgp-by-gens (t/full-ts-gens 4) t/mul))
(def S2 (sgp-by-gens (t/symmetric-gens 2) t/mul))
(def S3 (sgp-by-gens (t/symmetric-gens 3) t/mul))
(def T3 (sgp-by-gens (t/full-ts-gens 3) t/mul))
;; (def S6 (sgp-by-gens (t/symmetric-gens 6) t/mul))
;; (def T6 (sgp-by-gens (t/full-ts-gens 6) t/mul))


;; checking number of conjrepts
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
  ;(trace "COMPRESS" psols)
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

(compress [{:sources #{[0 1]}, :p {[1 0] [1 0]}, :last [1 0]}
           {:sources #{[1 0]}, :p {[1 0] [0 1], [0 1] [1 0]}, :last [0 1]}])

;decompressing
(defn decompress
  ([compressed] ; no basepoint given, we do all
   (if (contains? compressed :gens) 
     (let [basepoints (keys (:gens compressed))]
       (mapcat
        (partial decompress compressed)
        basepoints))
     [compressed]))
  ([{sources :sources p :p gens :gens} basepoint]
  ;(println sources p (keys gens))
   (into [{:sources sources :p p}] ;the one kept in the compressed format
         (map (fn [img]
                (let [hm {basepoint img, img basepoint}
                      hm+id (fn [arr] (hm arr arr))]
                  {:sources (set (map  hm+id sources))
                   :p (update-keys p hm+id)}))
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

(defn test-match-all-sources
  [t psol target]
  (match-all-sources
   (post-arrow t)
   (pre-arrow t)
   psol
   target))

(test-match-all-sources [0 1 0]
                        {:sources (set (t/single-maps [0 1 0])) :p {}}
                        [0 0])

(test-match-all-sources [3 3 3 3]
                        {:sources (set (t/single-maps [3 3 3 3])) :p {}}
                        [0 1])


(defn match-all
  "matching all partial solutions to a single target"
  [post-arrow pre-arrow psols target]
  (let [matching-groups
        (remove empty?
                (map
                 (fn [psol]
                   (match-all-sources post-arrow pre-arrow psol target))
                 psols))]
    ;(trace "MATCHINGS " (count matching-groups) (map count matching-groups))
    ;(trace "RAWMATCHINGS" matching-groups "LASTGROUPED" (group-by :last (apply concat matching-groups)))
    (map compress matching-groups)))

(defn test-match-all
  [t psols target]
  (match-all
   (post-arrow t)
   (pre-arrow t)
   psols
   target))

(test-match-all [0 1 0]
                [{:sources (set (t/single-maps [0 1 0])) :p {}}]
                [0 0])

(test-match-all [3 3 3 3]
                [{:sources (set (t/single-maps [3 3 3 3])) :p {}}]
                [0 1])

; doing [1 0 0]
(test-match-all [1 0 0]
                [{:sources (set (t/single-maps [1 0 0])) :p {}}]
                [0 0])   ;there should be none

(test-match-all [1 0 0]
                [{:sources (set (t/single-maps [1 0 0])) :p {}}]
                [0 1])     ;all maps should match

(test-match-all [1 0 0]
                (decompress {:sources #{[2 0] [0 1]}, :p {[1 0] [0 1]}, :gens {[1 0] [[2 0] [0 1]]}})
                [1 0])

(test-match-all [1 0 0]
                (mapcat decompress
                        [{:sources #{[2 0]}, :p {[1 0] [0 1], [0 1] [1 0]}}
                         {:sources #{[0 1]}, :p {[1 0] [1 0]}, :gens {[1 0] [[0 1]]}}
                         {:sources #{[2 0]}, :p {[1 0] [1 0]}, :gens {[1 0] [[2 0]]}}])
                [2 0])

(test-match-all-sources [1 0 0] {:sources #{[2 0]}, :p {[1 0] [0 1], [0 1] [1 0]}} [2 0] )
(test-match-all-sources [1 0 0] {:sources #{[1 0]}, :p {[1 0] [0 1]}} [2 0])

(defn info
  [t [k psols target]]
  (let []
    (println "Level " k "matched"
             ;(sort (vals p))
             "we have" (count psols) "partial solutions, trying" target)
    ;(pprint psols)
    (println "num of source sets" (count (set (map :sources (mapcat decompress psols)))))
    (pprint (group-by :sources (mapcat decompress psols)))
    ;(print "we get") 
    ;(pprint (match-all (post-arrow t) (pre-arrow t) (mapcat decompress psols) target))
    ;(println "We matched " (sort (vals (:p (first psols))))
    ;         "Compressed" (count (filter (comp identity :gens) psols)) "All same?" (= 1 (count (set (map (comp set vals :p) psols)))))
    ))


(defn conjugators
  "Direct construction of conjugacy class representative of transformation t."
  [t]
  (let [n (count t)
        pts (reverse (range n)) ;to make sure we start with zero (we use stack) so we get minimum
        sources (set (t/single-maps t))
        posts (post-arrow t)
        pres (pre-arrow t)
        match-all-fn (partial match-all posts pres)
        initial_stack (vec (for [pt pts]
                             [0
                              [{:sources sources :p {}}]
                              [0 pt]]))
        search (fn [stack]
                 (info t (peek stack))
                 ;(timbre/trace "stack!"  (reverse stack) "\n")
                 ;(trace "PEEK STACK" (peek stack))
                 (let [[k psols target] (peek stack)
                       allpsols (mapcat decompress psols)

                       npsols (match-all-fn allpsols target)]
                   ;(trace "MATCHING" (mapcat decompress psols) "to" target "GIVES" npsols "X")
                   (if (empty? npsols)
                     (recur (pop stack))
                     (if (empty? (:sources (first npsols)))
                       npsols
                       (recur (into (pop stack)
                                    (for [pt pts]
                                      [(inc k)
                                       npsols
                                       [(inc k) pt]])))))))]
    (search initial_stack)))

(defn conjrep
  [t]
  (mapv second (sort  (vals (:p (first (conjugators t)))))))


(conjugators [0 1 0])
(conjrep [0 1 0])
(conjugators [1 1])
(conjugators [0 0 0])

(conjugators [2 2 2])
(conjrep [2 2 2])

(conjugators [3 3 3 3])

(conjugators [1 1 1 2])
(count (conjugators [2 2 2 2 2 2]))
(t-c/conjrep [1 1 1 2])



(conjrep [1 2 3 4 0])
(t-c/conjrep [1 2 3 4 0])


(conjrep [1 0 0])
(t-c/conjrep [1 0 0])

;checking against the library
(filter
 #(not (= (t-c/conjrep %)
          (conjrep %)))
 (selections (range 5) 5))

(conjrep [1 2 1])
(t-c/conjrep [1 2 1])
(conjrep [2 2 0])