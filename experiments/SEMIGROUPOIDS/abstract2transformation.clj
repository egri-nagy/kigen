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
(println (first (isomorphisms S T)))
;(println (count (isomorphisms S T)))


;;connected one way
(def T2 (comptab (full-semigroupoid [[0 0] [0 1] [1 1]] [2 2])))
(println (first (isomorphisms S T2)))
;;(println (count (isomorphisms S T2)))

;; not connected, no embeddings
(def T3 (comptab (full-semigroupoid [[0 0] [1 1]] [2 2])))
(println (first (isomorphisms S T3)))
;;(println (count (isomorphisms S T3)))

;; with the strict moprhism, it is not expected to have embeddings here
(def T4 (comptab (full-semigroupoid [[0 0]] [3])))
(println (first (isomorphisms S T4)))
;;(println (count (isomorphisms S T4)))

; KIGEN 25.07.02 Clojure 1.12.1 Java 24.0.1 Mac OS X 15.5 aarch64
;[1 2 4 5 6 12]
;[1 2 4 5 6 8]
;nil
;[5 7 0 3 1 2]