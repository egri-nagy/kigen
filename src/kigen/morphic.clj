(ns kigen.morphic
  "The most abstract description of checking whether a relation is morphic
   or not.")

(defn morphic?
  "Decides whether `morph-fn` is a morphic relation for elements `a` and `b`.
  `source-composition-fn` binary composition operation in the source domain
  `target-composition-fn` same in the target domain
  `eq-fn` equality test in the target domain (= for homomorphisms,
   subset? for relational morphisms)
  This abstract function is not meant to be used directly, but for creating
  custom tests by pre-filling the arguments by partial."
  [eq-fn
   source-composition-fn
   target-composition-fn
   morph-fn
   a
   b]
  ;;(println morph-fn ":" a ":" b "gives" (source-composition-fn a b))
  (let [mab (morph-fn (source-composition-fn a b))]
    (or
     (nil? mab) ;in case it is partial
     (eq-fn (target-composition-fn (morph-fn a)
                                   (morph-fn b))
            mab))))

(defn morphism?
  "Decides whether the (partial) mapping morph-fn from the source domain `source`
   to the target domain `target` is a morphism or not."
  [eq-fn
   source-composition-fn
   target-composition-fn
   morph-fn
   source
   target]
  (every? identity
          (for [a source
                b target]
            ((partial morphic?
                      eq-fn
                      source-composition-fn
                      target-composition-fn
                      morph-fn)
             a b))))
