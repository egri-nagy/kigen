(ns kigen.morph2
  "Constructing morphisms by generators."
  (:require [kigen.multab :as multab :refer [at]]
            [clojure.set :refer [subset? difference]]
            [clojure.math.combinatorics :refer [subsets partitions]]))

(declare m2)

(defn m2
  "phi - morphism represented as a map
  x - element in frontline
  s - generator of S"
  [phi x s mulS mulT]
  (let [xs (mulS x s)
        XS (mulT (phi x) (phi s))]
    (if (contains? phi xs)
      (if (= XS (phi xs))
        {}
        nil)
      {xs XS})))
