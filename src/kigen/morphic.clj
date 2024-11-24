(ns kigen.morphic
  "The most abstract description of checking whether a relation is morphic
   or not."
  (:require [clojure.set :refer [subset?]]))

(defn morphic?
  "Decides whether `morph-fn` is a morphic relation for elements `a` and `b`.
  `source-composition-fn` binary composition operation in the source domain
  `target-composition-fn` same in the target domain
  `eq-fn` equality test in the target domain
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

(def relmorphic?
  "For relational morphisms the compatibility condition uses subset of." 
  (partial morphic? subset?))

(def homomorphic?
  "Homomorphims require equality."
  (partial morphic? =))

