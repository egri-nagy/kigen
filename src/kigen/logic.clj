(ns kigen.logic
  "Further relations (goals) to extend core.logic."
  (:require [clojure.core.logic :as l]))

(declare reduceo
         ntho)

(l/defne reduceo
  "Relational reduce, succeds if the reduction produces the result."
  [relo initial coll result]
  ([_ _ () _] (l/== initial result))
  ([_ _ [next-item . remaining] _]
   (l/fresh [result-so-far]
            (relo initial next-item result-so-far)
            (reduceo relo result-so-far remaining result))))

(defn ntho
  "Succeeds if the collection has x as the index n element.
   Classic recursive implementation."
  ([coll n x] (ntho coll n x 0))
  ([coll n x current]
   (l/fresh [head tail]
            (l/firsto coll head)
            (l/resto coll tail)
            (l/conde [(l/== head x) (l/== n current)]
                     [(ntho tail n x (inc current))]))))