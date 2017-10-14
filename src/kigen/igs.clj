(ns kigen.igs
  (:require [clojure.data.int-map :refer [int-set union difference]]
            [kigen.multab :as multab]))

(defn independent?
  "Decides whether the set A is an independent set in multiplication table mt."
  [mt A]
  (every?
   (fn [a]
     (not (multab/in-closure? mt (difference A (int-set [a])) a)))
   A))

(defn min-extensions
  "All independent sets that are extensions of the independent set I."
  [mt I]
  (let [complement (difference  (multab/elts mt) I)]
    (filter (partial independent? mt)
            (map (partial conj I) complement))))
