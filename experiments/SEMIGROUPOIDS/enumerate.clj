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
     "\nup to (anti)isomorphism:" (count assoc-typed-iso) "\n")))

(stats 1)
(stats 2)
(stats 3) ;; this is slow, maybe the type inference, or the morphisms?

;; ; KIGEN 25.06.xx Clojure 1.12.1 Java 24.0.1 Mac OS X 15.5 aarch64
;; All composition tables:  2
;; Associative composition tables: 2
;; up to (anti)isomorphism: 2
;; Typed: 2
;; up to (anti)isomorphism: 2
;; Assoc-Typed: 2
;; up to (anti)isomorphism: 2
;; All composition tables:  81
;; Associative composition tables: 20
;; up to (anti)isomorphism: 10
;; Typed: 24
;; up to (anti)isomorphism: 11
;; Assoc-Typed: 16
;; up to (anti)isomorphism: 8
;; All composition tables:  262144
;; Associative composition tables: 442
;; up to (anti)isomorphism: 67
;; Typed: 19943
;; up to (anti)isomorphism: 1769
;; Assoc-Typed: 271
;; up to (anti)isomorphism: 41