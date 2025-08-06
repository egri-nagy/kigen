(ns kigen.diagram.transf
  "Transformations and permutations simply representated as vectors."
  (:require [kigen.position :as pos]))

;; STANDARD GENERATING SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn idmap [n] (vec (range n)))
(defn transposition [n] (vec (concat [1 0] (range 2 n))))
(defn ncycle [n] (vec (concat (range 1 n) [0])))
(defn collapsing [n] (vec (concat [0 0] (range 2 n))))

(defn cyclic-gens [n] [(ncycle n)])

(defn symmetric-gens
  "Generators of the symmetric group of degree n using the embedding
  into the partitioned binary relation monoid defined by f."
  [n]
  (cond (= 1 n) [[0]]
        (= 2 n) [(transposition n)]
        :else [(ncycle n) (transposition n)]))

(defn full-ts-gens
  "Generators of the full transformation semigroup of degree n."
  [n]
  (if (= 1 n)
    (symmetric-gens n)
    (concat (symmetric-gens n) [(collapsing n)])))

(defn pts-gens
  "Generators of the partial transformation semigroup of degree n."
  [n]
  (let [ftsg (full-ts-gens n)]
    (concat (map #(conj % n) ftsg)
            [(vec (concat [n] (range 1 n) [n]))])))

(defn sym-inv-gens
  "Generators of the symmetric inverse monoid of degree n."
  [n]
  (let [ftsg (symmetric-gens n)]
    (concat (map #(conj % n) ftsg)
            [(vec (concat [n] (range 1 n) [n]))])))

(defn mul
  "Right multiplication of transformations represented by vectors."
  [s t]
  (mapv t s)) ; as simple as that

(defn act
  "Transformation t acting on a set of points."
  [points t]
  (set (map t points)))

(defn ->transf
  [points action]
  (mapv (fn [p] (pos/index points (action p)))
        points))

;;TODO bit of confusion, since this should in the permutation namespace,
;; but that is still PBR
(defn inverse
  "Inverse of a bijective transformation."
  [t]
  (let [pts (range (count t))]
    (mapv (zipmap t pts) pts)))

(defn conjugate-by-definition
  "The conjugate of a transformation by a permutation according to the
  definition, i.e. multiplying by inverse on the left and p on the right."
  [t p]
  (mul (mul (inverse p) t) p))

(defn conjugate
  "The conjugate of a transformation by direct relabeling according to p."
  [t p]
  (let [pts (range (count t))]
    (mapv (zipmap (map p pts) (map p t)) pts)))