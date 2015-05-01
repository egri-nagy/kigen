(ns kigen.pbr
  (:use [clojure.set :only [difference]]))

;; partitioned binary relations stored as maps: integers -> set of integers
;; e.g. {1 #{1 2}, 2 #{2}}
;; for degree n, domain is 1..n, codomain is n+1..2n
;; e.g. degree 3, domain is {1,2,3}, codomain is {4,5,6}

;; RANDOM PARTITIONED BINARY RELATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare rand-pbr
         rand-subset)

;; Fabricates an m,n partitioned binary relation.
;; just zipping the nodes with random subsets
;; not a good sampling at the moment
(defn rand-pbr
  "a random (n,m) paritioned binary relation"
  [m n]
  (let [N (+ m n 1) ;the number of points/nodes
        X (range 1 N) ;the full set of points, union of dom, cod
        pbr {:dom (set (range 1 (inc m))) ; intial map contains dom, cod
             :cod (set (range (inc m) N ))}]
    (into pbr (zipmap
               X
               (take (dec N) (repeatedly (partial rand-subset X)))))))

;; Returns a random subset of the given collection.
;; generating a vector of booleans as a characteristic function
;; 1. generate a random bitlist of length of the size of the collection
;; 2. pair the bits with the collection elements and filter the 'true pairs
;; 3. return the set of the element parts of the filtered pairs
(defn rand-subset
  "returns a random subset of the given collection"
  [coll]
  (let [bits (take (count coll) (repeatedly (partial rand-nth [true false])))]
    (set (map second (filter first (map list bits coll))))))

;; MULTIPLICATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shifting up the integer points by a given value (needed by multiplication)
;; the shifting is specified by a map, e.g.  {:dom 0 :cod 2} means not to
;; touch the domain but shift the codomain by 2
(defn shift-pbr
  [pbr offsets]
  (let [shift-point (fn [point] (cond ((:dom pbr) point)
                                     (+ point (:dom offsets))
                                     ((:cod pbr) point)
                                     (+ point (:cod offsets))
                                     :else point))
        shift-set (fn [X] (set (map shift-point X)))]
    (reduce (fn [m [k v]] (conj m [(shift-point k) (shift-set v)])) {} pbr)) )

;; the edges of the given pbr from the given node
;; simple lookup in the map and putting the edges in 2-vectors
(defn edges-from-node
  [node pbr]
  (set (map #(vector node %) (pbr node))))

;; the union of all edges in the pbr starting from the given nodes
(defn edges-from-nodes
  [nodes pbr]
  (reduce into #{} (map #(edges-from-node % pbr) nodes)))

;; extracting the set of target nodes from a coll of edges
;; i.e. getting the second elements of the pairs
(defn targets [edges] (set (map #(last %) edges)))

;; orbit is a map with :all edges discovered and a vector containing
;; sets of edges :graded by the number of steps after they got collected
;; TODO this one is not suitable for a generic orbit algorithm due to pbrs
(defn orbit-seq
  [orbit pbr flip]
  (cons orbit
        (lazy-seq (orbit-seq
                   (let [{:keys [all graded]} orbit
                         diff (difference
                               (edges-from-nodes
                                (targets (last graded))
                                pbr)
                               all)]
                     {:all (into all diff) :graded (conj graded diff)})
                   (flip pbr) flip))))

(defn orbit
  [i pbr flip]
  (let [A (edges-from-node i pbr)]
    (orbit-seq {:all A :graded [A]} (flip pbr) flip)))

(defn image-set
  [node pbr flip endpoints]
  ((comp (partial filter endpoints) targets :all last)
   (take-while
    #(not (empty?(last (:graded %))))
    (orbit node pbr flip))))

(defn mul
  "multiply two partitioned binary relations"
  [a b]
  (let [offset (count (:dom a))
        b# (shift-pbr b {:dom offset :cod offset})
        ab# {:dom (:dom a) :cod (:cod b#)}
        endpoints (into (:dom ab#) (:cod ab#))
        flip {a b#, b# a}] ;map for switching between pbrs
    (shift-pbr
     (into ab#
           (mapcat
            (fn [points pbr]
              (map
               #(vector % (image-set % pbr flip  endpoints))
               points))
            [(:dom ab#),(:cod ab#)]
            [a,b#]))
     {:dom 0 :cod (- (count (:dom b)))})))
