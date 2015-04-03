(ns kigen.pbr)

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

(defn mul
  "multiply two partitioned binary relations"
  [alpha beta]
  )
