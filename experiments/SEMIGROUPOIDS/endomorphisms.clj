;; finding endomorphisms of an abstract semigroupoid
;; v25.06.xx
(require '[kigen.semigroupoid.homomorphism :refer [isomorphisms
                                                   homomorphisms
                                                   subtable]])
(require '[kigen.semigroupoid.transformation :refer [symbol-comptab]])
(require '[kigen.semigroupoid.enumeration :refer [associativity?
                                                  find-minimal-type-structure
                                                  typestruct2arrows]])
(require '[kigen.semigroupoid.viz :refer [DotSemigroupoid
                                          Dot2PDF]])

;; Example 3.2 from Representation Independent Decompositions of Computation https://arxiv.org/abs/2504.04660
(def S
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])

(defn print-symbol-comptab
  [s]
  (doseq [row s]
    (println row)))

(print-symbol-comptab (symbol-comptab S))

(def homs (homomorphisms S S))

(def images (map (partial subtable S) homs))

(every? associativity? images)

(doseq [i images]
  (let [arrows (typestruct2arrows (find-minimal-type-structure i))]
    (print-symbol-comptab (symbol-comptab i))
    (Dot2PDF (DotSemigroupoid i arrows) (str "S" i))
    (println "arrows:" arrows)))