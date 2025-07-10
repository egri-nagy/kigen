;; finding endomorphisms of an abstract semigroupoid
;; v25.06.xx
(require '[kigen.semigroupoid.homomorphism :as hom])
(require '[kigen.semigroupoid.transformation :as t])
(require '[kigen.semigroupoid.enumeration :as enum])
(require '[kigen.semigroupoid.viz :refer [DotSemigroupoid
                                          Dot2PDF]])
(require '[kigen.logic :refer [lvar-table lvar-vector]])
(require '[kigen.semigroup.conjugacy :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as transf])


(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :refer [selections]])

(l/run*
 [q r]
 (l/membero q ["linux" "windows" "mac" "android" ""])
 (l/conda
  [(l/membero q ["linux" "windows"]) (l/== r 1)]
  [(l/== q "mac") (l/== r 2)]
  [l/succeed (l/== q "") (l/== r 3)]))

(l/run*
 [q r]
 (l/conda
  [(l/membero q ["linux" "windows"]) (l/== r 1)]
  [(l/== q "mac") (l/== r 2)]
  [l/succeed (l/== q "") (l/== r 3)])
 (l/membero q ["linux" "windows" "mac" "android" ""]))

(defn types
  "`n` arrows, `m` types"
  [n m]
  (let [[arrows lvars] (lvar-table n 2)
        pairs (for [a arrows, b arrows] [a b])
        objects (range m)]
    (l/run*
     [q]
     (l/== q arrows)
     ;;arrows have valid doms/cods
     (l/everyg #(fd/in % (fd/interval 0 (dec m))) lvars)
     ;;every type is used
     ;;(l/everyg #(l/membero % lvars) objects)
     (l/distincto arrows) ;no parralel arrows
     (l/everyg (fn [[[da ca] [db cb]]]
                 (l/conde
                  [(l/distincto [ca db])]
                  [(l/membero [da cb] arrows)]))
               pairs))))
(types 1 1)
(distinct (types 1 2))
(count (set (types 2 2)))
;(count (set (types 3 1)))
(count (set (types 3 3)))

;; testing out distincto, this would work well for the arrow-type semigroupoid
(l/run* [q]
        (l/fresh [a b]
                 (l/== q [a b])
                 (l/membero a [0 1 ])
                 (l/membero b [:a :b 1 2]) 
                 (l/distincto  (conj [[0 1] [1 2]] [a b]))))



(one-more-arrow [[0 0] [1 1] ] 2)

(def sol4-5 [[[0 0] [0 1] [0 2] [3 4]]
             [[0 0] [0 1] [2 1] [3 4]]
             [[0 0] [0 1] [2 2] [3 4]]
             [[0 0] [0 1] [2 3] [2 4]]
             [[0 0] [0 1] [2 3] [4 3]]
             [[0 0] [1 0] [1 2] [3 4]]
             [[0 0] [1 0] [2 0] [3 4]]
             [[0 0] [1 0] [2 2] [3 4]]
             [[0 0] [1 0] [2 3] [2 4]]
             [[0 0] [1 0] [2 3] [4 3]]
             [[0 0] [1 1] [2 2] [3 4]]
             [[0 0] [1 1] [2 3] [2 4]]
             [[0 0] [1 1] [2 3] [4 3]]
             [[0 0] [1 2] [1 3] [1 4]]
             [[0 0] [1 2] [1 3] [4 2]]
             [[0 0] [1 2] [3 2] [4 2]]
             [[0 1] [0 2] [0 3] [0 4]]
             [[0 1] [0 2] [0 3] [4 1]]
             [[0 1] [0 2] [1 2] [3 4]]
             [[0 1] [0 2] [3 1] [3 4]]
             [[0 1] [0 2] [3 1] [4 1]]
             [[0 1] [0 2] [3 1] [4 2]]
             [[0 1] [2 1] [3 1] [4 1]]])


;; Example 3.2 from Representation Independent Decompositions of Computation https://arxiv.org/abs/2504.04660
(def S
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])

;; weak isomorphism not symmetric
(hom/isomorphisms [[0 :n] [:n :n]]  [[0 1] [1 0]])
(hom/isomorphisms [[0 1] [1 0]] [[0 :n] [:n :n]])

(def non-symmetric-iso
  (filter (fn [T]
            (and
             (not (empty? (hom/isomorphisms T S)))
             (empty? (hom/isomorphisms S T))))
          (enum/all-composition-tables 2)))

;weak isomorphism not even producing semigroupoids
(count non-symmetric-iso)
(count (filter enum/associativity? non-symmetric-iso))

(count (hom/homomorphisms S S))
(count (hom/strict-homomorphisms S S))


