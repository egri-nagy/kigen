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


;; testing X's counterexample of (ab)c defined, but bc not
(def comptab '[0 0 0
               0 0 - 
               0 0 0])

(enum/semigroupoids-order-n 3 comptab)
(enum/associativity? [ [0 0 0] [0 :n 0] [0 0 0]])

;;really not allowing the stack use
(enum/associativity-failures
 [
  [3 4 5 :n :n 0 :n :n 1 :n :n :n]
  [6 7 8 :n :n 1 :n :n 0 :n :n :n]
  [9 10 11 :n :n :n :n :n :n :n :n :n]
  [:n :n 0 :n :n :n :n :n :n 3 4 5]
  [:n :n 1 :n :n :n :n :n :n 6 7 8]
  [:n :n :n :n :n :n :n :n :n :n :n :n]
  [:n :n 1 :n :n :n :n :n :n 6 7 8]
  [:n :n 0 :n :n :n :n :n :n 3 4 5]
  [:n :n :n :n :n :n :n :n :n :n :n :n]
  [:n :n :n :n :n :n :n :n :n :n :n :n]
  [:n :n :n :n :n :n :n :n :n :n :n :n]
  [:n :n :n :n :n :n :n :n :n :n :n :n] ])
;;fixing but not working
(count
 (enum/associativity-failures
  [[3 4 5 :n :n 0 :n :n 1 :n :n :n]
   [6 7 8 :n :n 1 :n :n 0 :n :n :n]
   [9 10 11 :n :n :n :n :n :n :n :n :n]
   [:n :n 0 :n :n 3 :n :n 4 3 4 5]
   [:n :n 1 :n :n 4 :n :n 3 6 7 8]
   [:n :n :n :n :n :n :n :n :n :n :n :n]
   [:n :n 1 :n :n 6 :n :n 7 6 7 8]
   [:n :n 0 :n :n 7 :n :n 6 3 4 5]
   [:n :n :n :n :n :n :n :n :n :n :n :n]
   [:n :n :n :n :n 9 :n :n 10 :n :n :n]
   [:n :n :n :n :n 10 :n :n 9 :n :n :n]
   [:n :n :n :n :n :n :n :n :n :n :n :n]]))


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
