(ns kigen.morphic
  "The most abstract description of checking whether a relation is morphic
   or not.
   morph-m  - morphisms are represented as hash-maps, thus they can be partial ")

(defn morphic?
  "Decides whether `morph-m` is a morphic relation for elements `a` and `b`.
  `source-composition-fn` binary composition operation in the source domain
  `target-composition-fn` same in the target domain
  `eq-fn` equality test in the target domain (= for homomorphisms,
  subset? for relational morphisms)
  This abstract function is not meant to be used directly, but for creating
  custom tests by pre-filling the arguments by partial.
  If the product ab in the source domain does not yet have a morphic image,
  then there is nothing to check, so it returns true, meaning that the
  we have a partial morphism, which potentially can be completed."
  [eq-fn
   source-composition-fn
   target-composition-fn
   morph-m
   a
   b]
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