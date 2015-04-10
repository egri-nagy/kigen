(ns kigen.pbr
  (:require [clojure.set :as set]))

;; partitioned binary relations stored as maps: integers -> set of integers
;; e.g. {1 #{1 2}, 2 #{2}}
;; for degree n, domain is 1..n, codomain is n+1..2n
;; e.g. degree 3, domain is {1,2,3}, codomain is {4,5,6}

;; generating a vector of booleans as a characteristic function
;; 1. generate a random bitlist of length of the size of the collection
;; 2. pair the bits with the collection elements and filter the 'true pairs
;; 3. return the set of the element parts of the filtered pairs
(defn rand-subset
  "returns a random subset of the given collection"
  [coll]
  (let [bits (take (count coll) (repeatedly (partial rand-nth [true false])))]
    (set (map second (filter first (map list bits coll))))))

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

;; shifting up the integer points by a given value (needed by multiplication)
(defn shift-set [X n]  (set (map #(+ n %) X)))

(defn sharp-pbr
  [pbr n]
  (reduce (fn [m [k v]]
            (if (keyword? k)
              (conj m [k (shift-set v n)])
              (conj m [ (+ k n) (shift-set v n)])))
          {} pbr))

(defn cond-shift-set [X n]  (set (map #(if (<= % n) % (- % n)) X)))

(defn flat-cod-pbr ; just identity for now
  [pbr n]
  (into {} (map (fn [[k v]] (if (or (keyword? k) (<= k n))
                             [k (cond-shift-set v n)]
                             [(- k n) (cond-shift-set v n)]))
                pbr)))

;; the edges of the given pbr from the given node
;; simple lookup in the map
(defn edges-from-node
  [node pbr]
  (set
   (map #(vector node %) (pbr node))))

;; the union of all edges in the pbr starting from the given nodes
(defn edges-from-nodes
  [nodes pbr]
  (apply set/union (map #(edges-from-node % pbr) nodes)))

;; extracting the set of target nodes from a coll of edges
(defn targets
  [edges]
  (set (map #(last %) edges)))

(defn orbit-seq
  [orbit pbrs]
  (cons orbit
        (lazy-seq (orbit-seq
                   (let [{:keys [all graded]} orbit
                         diff (set/difference
                               (edges-from-nodes
                                (targets (last graded))
                                (first pbrs))
                               all)]
                     {:all (into all diff) :graded (conj graded diff)})
                   (rest pbrs)))))

(defn orbit
  [i pbrs]
  (let [A (edges-from-node i (first pbrs))]
    (orbit-seq {:all A :graded [A]} (rest pbrs)) ))

(defn image-set
  [node pbrs endpoints]
  ((comp (partial filter endpoints) targets :all last)
   (take-while
    #(not (empty?(last (:graded %))))
    (orbit node pbrs))))

(defn mul
  "multiply two partitioned binary relations"
  [a b]
  (let [offset (count (:dom a))
        b# (sharp-pbr b offset)
        ab# {:dom (:dom a) :cod (:cod b#)}
        endpoints (set/union (:dom ab#) (:cod ab#)) ]
    (flat-cod-pbr
     (into ab#
           (mapcat
            (fn [points pbrs]
              (map
               #(vector % (image-set % pbrs  endpoints))
               points))
            [ (:dom ab#) (:cod ab#)]
            [(cycle [a b#]) (cycle [b# a])]))
     (count (:dom b)))))

(def a {:dom #{1 2} :cod #{3 4} 1 #{2 3} 2 #{} 3 #{2} 4 #{}})
(def b {:dom #{1 2} :cod #{3 4} 1 #{4} 2 #{3} 3 #{} 4 #{1}})
(def b# ( sharp-pbr b 2))

;; example from http://arxiv.org/abs/1102.0862
(def alpha {:dom #{1 2 3 4 5 6 7} :cod #{8 9 10 11 12 13 14 15 16}
            1 #{9} 2 #{} 3 #{4} 4 #{4} 5 #{} 6 #{13} 7 #{14}
            8 #{8} 9 #{1 10} 10 #{11} 11 #{} 12 #{2,12} 13 #{6}
            14 #{7} 15 #{16} 16 #{15}})
(def beta {:dom #{1 2 3 4 5 6 7 8 9} :cod #{10 11 12 13}
           1 #{} 2 #{10} 3 #{3} 4 #{5} 5 #{4 11} 6 #{6} 7 #{} 8 #{12} 9 #{}
           10 #{2} 11 #{} 12 #{8} 13 #{9}})
(def beta# (sharp-pbr beta (count (:dom alpha))))
(def alphabeta {:dom #{1 2 3 4 5 6 7} :cod #{8 9 10 11}
                1 #{8} 2 #{} 3 #{4} 4 #{4} 5 #{} 6 #{6} 7 #{}
                8 #{1 2 9} 9 #{} 10 #{} 11 #{10}})
