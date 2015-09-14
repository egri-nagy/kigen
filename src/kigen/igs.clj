(ns kigen.igs
  (:use [clojure.set :only [difference union]]
        [kigen.multab :as multab]))

(defn igs?
  "Decides whether the set A is an independent set in multiplication table M."
  [A M]
  (every?
   #(contains? (multab/closure mt (difference A #{%})) %)
   A))
