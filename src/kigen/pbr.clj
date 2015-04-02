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

(defn rand-pbr
  "a random (n,m) paritioned binary relation"
  [m n]
  (let [ pbr {:dom (set (range 1 (inc m)))
              :cod (set (range (inc m) (+ m n 1)))}]
    (reduce conj pbr (take 5 (repeat [1 #{1}])))
    ))
