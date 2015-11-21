(ns kigen.pbr
  (:use [clojure.set :only [difference union]]
        [kigen.orbit :only [dfs]]))

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
  "a random (n,m) partitioned binary relation"
  [m n]
  (let [N (+ m n 1) ;the number of points/nodes
        X (range 1 N) ;the full set of points, union of dom, cod
        pbr {:dom (set (range 1 (inc m))) ; initial map contains dom, cod
             :cod (set (range (inc m) N ))}]
    (into pbr (zipmap
               X
               (take (dec N) (repeatedly #(rand-subset X)))))))

;; Returns a random subset of the given collection.
;; generating a vector of booleans as a characteristic function
;; 1. generate a random bitlist of length of the size of the collection
;; 2. pair the bits with the collection elements and filter the true pairs
;; 3. return the set of the element parts of the filtered pairs
(defn rand-subset
  "returns a random subset of the given collection"
  [coll]
  (let [bits (take (count coll) (repeatedly #(rand-nth [true false])))]
    (set (map first (filter second (zipmap coll bits))))))

;; MULTIPLICATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare mul
         act
         img)

;; shifting up the integer points by a given value (needed by multiplication)
;; the shifting is specified by a map, e.g.  {:dom 0 :cod 2} means not to
;; touch the domain but shift the codomain by 2
(defn shift-pbr
  [pbr offsets]
  (letfn [(shift-point [point]
            (cond ((:dom pbr) point) (+ point (:dom offsets))
                  ((:cod pbr) point) (+ point (:cod offsets))
                  :else point))
          (shift-set [X] (set (map shift-point X)))]
    (reduce (fn [m [k v]] (conj m [(shift-point k) (shift-set v)])) {} pbr)))

;; the edges of the given pbr from the given node
;; simple look-up in the map and putting the edges in 2-vectors
(defn edges-from-node
  [node pbr]
  (map #(vector node %) (pbr node)))

;; we do a breadth first search in the combined graph of the pbrs
;; we need to take edges alternating, so we have to work with labelled edges
;; where the label tells in which pbr the edge exist
(defn reachable-endpoints
  [node pbrs endpoints]
  (let [flipper {0 1, 1 0} ;map for switching the labels
        seeds (map vector (repeat 0) (edges-from-node node (first pbrs)))
        ;the action function, generating labelled edges
        af (fn [[i edge]]
             (let [j (flipper i)]
               (map vector
                    (repeat j)
                    (edges-from-node (second edge) (nth pbrs j)))))
        ;to extract the target nodes from labelled edges
        targets (fn [edges] (set (map #(last (last %)) edges)))]
    (when-not (zero? (count seeds))
      (filter endpoints (targets (dfs seeds af))))))

(defn mul
  "multiply two partitioned binary relations"
  [a b]
  (let [offset (count (:dom a))
        b# (shift-pbr b {:dom offset :cod offset})
        ab# {:dom (:dom a) :cod (:cod b#)}
        endpoints (into (:dom ab#) (:cod ab#))]
    (shift-pbr
     (into ab#
           (mapcat
            (fn [points pbrs]
              (map #(vector % (reachable-endpoints % pbrs endpoints)) points))
            [(:dom ab#) (:cod ab#)]
            [[a,b#] [b#,a]]))
     {:dom 0 :cod (- (count (:dom b)))})))

(defn act
  "the action of a partitioned binary relation on a set
  that is a subset of the union of its domain and codomain"
  [set pbr]
  (reduce into #{} (for [x set] (pbr x))))

(defn img
  "the image of the partitioned binary relation, i.e. acting on its points"
  [pbr]
  (act (into (:dom pbr) (:cod pbr)) pbr))

(defn flip
  "Flips a pbr upside-down."
  [pbr]
  (let [dom (:dom pbr)
        d (count dom)
        cod (:cod pbr)
        c (count cod)
        f (fn [x] (cond (contains? dom x) (+ x c)
                        (contains? cod x) (- x d)
                        :else (x {:dom :cod, :cod :dom})))]
    (reduce #(conj % [(f %2), (set (map f (pbr %2)))]) {} (keys pbr))))

(defn rev
  "Reverses the arrow in a pbr."
  [pbr]
  (let [points (union (:dom pbr) (:cod pbr))
        f (fn [x] (set (filter #(contains? (pbr %) x) points)))
        imgs (map #(vector % (f %)) points)]
    (into {:dom (:dom pbr), :cod (:cod pbr)} imgs)))

(defn overlay
  "Overlays two pbrs. The pbrs should have the same (co)domains."
  [pbr1 pbr2]
  (let [points (union (:dom pbr1) (:cod pbr1))]
    (into {:dom (:dom pbr1), :cod (:cod pbr1)}
          (map #(vector % (union (pbr1 %) (pbr2 %))) points)))
  )
