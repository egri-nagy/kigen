;; finding transformation representation of an abstract semigroupoid
;; v25.06.xx
(require '[kigen.semigroupoid.homomorphism :refer [isomorphisms]])
(require '[kigen.semigroupoid.transformation :refer [comptab
                                                     full-semigroupoid]])

;; Example 3.2 from Representation Independent Decompositions of Computation https://arxiv.org/abs/2504.04660
(def S
  [[0 1 2 3 4 nil]
   [1 0 2 4 3 nil]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 5]])

;; two objects with two states fully connected
(def T (comptab (full-semigroupoid [[0 0] [0 1] [1 1] [1 0]] [2 2])))
(count (isomorphisms S T))

;;connected one way
(def T2 (comptab (full-semigroupoid [[0 0] [0 1] [1 1]] [2 2])))
(count (isomorphisms S T2))

;; not connected, no embeddings
(def T3 (comptab (full-semigroupoid [[0 0] [1 1]] [2 2])))
(count (isomorphisms S T3))

(def T4 (comptab (full-semigroupoid [[0 0]] [3])))
(count (isomorphisms S T4))

