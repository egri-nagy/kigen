(ns kigen.subsgp
  "General functions for computing subsemigroups.
  Black box style, the element(s) and the operation need to be supplied."
  (:require [clojure.math.combinatorics :refer [selections]]
            [orbit.core :refer [full-orbit]]
            [orbit.action :refer [right-action right-actions set-action]]
            [kigen.memory-info :refer [mem-info]]
            [taoensso.timbre :refer [info]]))


(defn subsgp-by-closure
  "Adding a single new generator. S - subsemigroup, gen - a new generator,
  mul - the semigroup's binary operation."
  [S gen mul]
  (full-orbit (conj S gen)
              (set-action [(right-action mul gen)])))
