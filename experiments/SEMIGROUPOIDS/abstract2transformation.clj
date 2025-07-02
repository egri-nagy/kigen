;; finding transformation representation of an abstract semigroupoid
;; v25.07.02
(require '[kigen.semigroupoid.homomorphism :refer [isomorphisms]])
(require '[kigen.semigroupoid.transformation :refer [comptab
                                                     full-semigroupoid]])

;; Example 3.2 from Representation Independent Decompositions of Computation https://arxiv.org/abs/2504.04660
(def S
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])

;; two objects with two states fully connected
(def T (comptab (full-semigroupoid [[0 0] [0 1] [1 1] [1 0]] [2 2])))
(count (isomorphisms S T))

;;connected one way
(def T2 (comptab (full-semigroupoid [[0 0] [0 1] [1 1]] [2 2])))
(count (isomorphisms S T2))

;; not connected, no embeddings
(def T3 (comptab (full-semigroupoid [[0 0] [1 1]] [2 2])))
(count (isomorphisms S T3))

;; with the strict moprhism, it is not expected to have embeddings here
(def T4 (comptab (full-semigroupoid [[0 0]] [3])))
(count (isomorphisms S T4))

