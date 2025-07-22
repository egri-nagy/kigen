(ns kigen.morphic
  "The most abstract description morphic relations and functions.
   morph-m  - called 'morphism map', morphisms are represented as hash-maps,
   thus they can be partially defined")

(defn morphic?
  "Decides whether `morph-m` is a morphic relation for elements `a` and `b`.
  More descriptive names would be potentially-morphic?, or morphic-so-far?, as
  we need a defined incompatibility to refute, partial defaults to morphic.
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
   a b]
  (let [mab (morph-m (source-composition-fn a b))]
    (or
     (nil? mab) ;if morph-m is partial then we consider it potentially morphic
     (eq-fn (target-composition-fn (morph-m a) (morph-m b))
            mab)))) ;when defined, they should match

(defn morphism?
  "Decides whether the (partial) mapping `morph-m` from the source domain
   to the target domain is a morphism or not.
   Target domain does not need to be defined, as elements in the target are
   obtained as images of source elements.
   It lazily checks all pairs in the source for which the morphism is defined."
  [eq-fn
   source-composition-fn
   target-composition-fn
   morph-m]
  (let [sourcedom (keys morph-m)
        m? (partial morphic?
                    eq-fn
                    source-composition-fn
                    target-composition-fn
                    morph-m)]
    (every? identity ;we simply check for truthiness
            (for [a sourcedom
                  b sourcedom]
              (m? a b)))))