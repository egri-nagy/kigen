;; constructing abstract semigroupoids
;; v25.06.xx
(require '[kigen.semigroupoid.enumeration
           :refer [semigroupoids-order-n
                   all-composition-tables
                   associativity?
                   type-degree
                   find-minimal-type-structure]])
(require '[kigen.semigroupoid.homomorphism
           :refer [composition-relation
                   isomorphisms
                   comptabs-up-to-morphisms]])

(defn stats
  [n]
  (let [assoc-comptabs (filter
                        associativity?
                        (all-composition-tables n))
        assoc-comptabs-iso (comptabs-up-to-morphisms assoc-comptabs)]
    (println
     "Associative composition tables:" (count assoc-comptabs)
     "\nup to (anti)isomoprhism:" (count assoc-comptabs-iso))))

(stats 3)

(comptabs-up-to-morphisms (filter
                           associativity?
                           (all-composition-tables 2)))

(isomorphisms [[0 :n] [:n :n]]
              [[:n :n] [:n 1]])


(isomorphisms [[:n :n] [:n 1]]
              [[0 :n] [:n :n]])

(homomorphisms [[:n :n] [:n 1]]
               [[0 :n] [:n :n]])


(filter #(not (empty? (homomorphisms % [[0 :n] [:n :n]])))
        (all-composition-tables 2))

(composition-relation [[0 :n] [:n :n]])

(type-degree [[:n :n :n] [:n :n :n] [:n :n :n]])
