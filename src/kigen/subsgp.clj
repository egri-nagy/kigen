(ns kigen.subsgp
  "General functions for computing subsemigroups.
  Black box style, the element(s) and the operation need to be supplied."
  (:require [clojure.math.combinatorics :refer [selections]]
            [orbit.core :refer [full-orbit-single-op full-orbit]]
            [orbit.action :refer [right-action]]
            [kigen.memory-info :refer [mem-info]]
            [taoensso.timbre :refer [info]]))


(defn subsgp-closure
  "Adding a single new generator. S - subsemigroup, gen - a new generator,
  mul - the semigroup's binary operation."
  [S gen mul]
  (full-orbit-single-op (conj S gen)
                        (right-action mul gen)))

(defn min-extensions
  "Returns the minimal extensions (by new element) of closed subarray of
  multiplication table mt."
  [S mul closedsub]
  (let [X (remove closedsub S)]
    (println X)
    (set (map #(subsgp-closure closedsub % mul) X))))


(defn subsgps
  "All subsemigroups of the given semigroup. Subsemigroups will be "
  [S mul]
  (full-orbit [(empty S)] ;keeping collection type
              (partial min-extensions S mul)))
