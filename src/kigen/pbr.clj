(ns kigen.pbr
  (:require [clojure.set :as set]))

;; partitioned binary relations stored as maps: integers -> set of integers
;; e.g. {1 #{1 2}, 2 #{2}}
;; for degree n, domain is 1..n, codomain is n+1..2n
;; e.g. degree 3, domain is {1,2,3}, codomain is {4,5,6}

; generating a vector of booleans as a characteristic function
(defn rand-subset
  "returns a random subset of the given collection"
  [coll]
  (let [bits (take (count coll) (repeatedly (partial rand-nth [true false])))]
    (set (map second (filter first (map list  bits coll))))))

; not a good sampling at the moment
(defn rand-pbr
  "a random (n,m) paritioned binary relation"
  [m n]
  (let [N (+ m n 1)
        X (range 1 N) ;the full set of points, union of cod, dom
        pbr {:dom (set (range 1 (inc m)))
             :cod (set (range (inc m) N ))}] ; intial map contains dom, cod
    (into pbr (zipmap
               X
               (take (dec N) (repeatedly (partial rand-subset X)))))))

;; shifting up the integer points by a given value (needed by multiplication)
(defn sharp-pbr
  [pbr n]
  (let [f (fn [X] (set (map #(+ n %) X)))] ; shifting sets
    (reduce (fn [m [k v]]
               (if (keyword? k)
                 (conj m [k (f v)])
                 (conj m [ (+ k n) (f v)])))
            {} pbr)))

(defn flat-cod-pbr ; just identity for now
  [pbr n]
  pbr)

(defn foo
  [i ll] ;lazy list of pbrs
  (last (take-while #(not ( empty? (last (:orbit %))))
                    (reductions (fn [m pbr]
                                  (let [diff (set/difference (reduce set/union (map pbr (last (:orbit m)))) (:visited m))]
                                        ; (println diff)
                                    {:visited (into (:visited m) diff)
                                     :orbit (conj (:orbit m) diff)}))
                                {:visited ((first ll) i) :orbit [((first ll) i)]}
                                ll)))
  )

(defn mul
  "multiply two partitioned binary relations"
  [a b]
  (let [offset (count (:dom a))
        b# (sharp-pbr b offset)
        ab# {:dom (:dom a) :cod (:cod b#)}]
    (foo 1 (cycle [a b#]))
    ;(flat-cod-pbr ab#  offset)
    ))
