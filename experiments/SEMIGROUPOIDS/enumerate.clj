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
        assoc-comptabs-iso (comptabs-up-to-morphisms assoc-comptabs)
        typed-comptabs (filter
                        (fn [S]
                          (not (empty? (find-minimal-type-structure S))))
                        (all-composition-tables n))
        typed-comptabs-iso (comptabs-up-to-morphisms typed-comptabs)
        assoc-typed (filter (set assoc-comptabs) typed-comptabs)
        assoc-typed-iso (comptabs-up-to-morphisms assoc-typed)]
    (println
     "All composition tables: " (count (all-composition-tables n))
     "\nAssociative composition tables:" (count assoc-comptabs)
     "\nup to (anti)isomorphism:" (count assoc-comptabs-iso)
     "\nTyped:" (count typed-comptabs)
     "\nup to (anti)isomorphism:" (count typed-comptabs-iso)
     "\nAssoc-Typed:" (count assoc-typed)
     "\nup to (anti)isomorphism:" (count assoc-typed-iso))))

(stats 1)
(stats 2)
(stats 3) ;; this is slow, maybe the type inference?