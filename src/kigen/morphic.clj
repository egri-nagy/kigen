(ns kigen.morphic
  "The most abstract description of checking whether a relation is morphic
   or not.
   morph-m  - (partial) morphisms are represented as hash-maps")

(defn morphic?
  "Decides whether `morph-m` is a morphic relation for elements `a` and `b`.
  `source-composition-fn` binary composition operation in the source domain
  `target-composition-fn` same in the target domain
  `eq-fn` equality test in the target domain (= for homomorphisms,
   subset? for relational morphisms)
  This abstract function is not meant to be used directly, but for creating
  custom tests by pre-filling the arguments by partial."
  [eq-fn
   source-composition-fn
   target-composition-fn
   morph-m
   a
   b]
  ;;(println morph-m ":" a ":" b "gives" (source-composition-fn a b))
  (let [mab (morph-m (source-composition-fn a b))]
    (or
     (nil? mab) ;in case it is partial
     (eq-fn (target-composition-fn (morph-m a)
                                   (morph-m b))
            mab))))

(defn morphism?
  "Decides whether the (partial) mapping morph-m from the source domain `source`
   to the target domain is a morphism or not.
   Target domain does not need to be defined, as elements in the target are
   obtained as images of source elements."
  [eq-fn
   source-composition-fn
   target-composition-fn
   morph-m]
  (let [sourcedom (keys morph-m)]
    (every? identity
            (for [a sourcedom
                  b sourcedom]
              ((partial morphic?
                        eq-fn
                        source-composition-fn
                        target-composition-fn
                        morph-m)
               a b)))))
