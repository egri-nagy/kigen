(ns kigen.pbr)

;; partitioned binary relations stored as maps: integers -> set of integers
;; e.g. {1 #{1 2}, 2 #{2}}
;; for degree n, domain is 1..n, codomain is n+1..2n
;; e.g. degree 3, domain is {1,2,3}, codomain is {4,5,6}

(defn cod-cod
  "the relation within the codomain only"
  [pbr k]
  nil)
