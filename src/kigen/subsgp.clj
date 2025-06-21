(ns kigen.subsgp
  "General functions for computing subsemigroups.
  Black box style, the element(s) and the operation need to be supplied."
  (:require   [orbit.core :refer [full-orbit]]
              [kigen.semigroup.action :refer [right-action]]))

(defn new-elts
  "Takes a set and an element and returns the closure with that element
   and the set of new elements."
  [A g mul]
  (let [A' (conj A g)
        A'g (set (concat (map (right-action mul g) A')
                         (map (partial mul g) A')))
        newelts (remove A' A'g)]
    [(into A' newelts) newelts]))


(defn subsgp-closure
  "Adding a single new generator. S - subsemigroup, gen - a new generator,
  mul - the semigroup's binary operation."
  [S g mul]
  (loop [elts S
         newelts [g]]
    (if (empty? newelts)
      elts
      (let [[S' n'] (new-elts elts (first newelts) mul)]
        (recur S' (into n' (rest newelts)))))))

(defn min-extensions
  "Returns the minimal extensions (by new element) of closed subarray of
  multiplication table mt."
  [S mul closedsub]
  (let [X (remove closedsub S)]
    (set (map #(subsgp-closure closedsub % mul) X))))


(defn subsgps
  "All subsemigroups of the given semigroup. Subsemigroups will be "
  [S mul]
  (full-orbit [(empty S)] ;keeping collection type
              (partial min-extensions S mul)))
