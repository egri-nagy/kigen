(ns kigen.logic
  "Further relations (goals) and utility functions to extend core.logic."
  (:require [clojure.core.logic :as l]))

(declare reduceo
         ntho
         lvar-table)

(l/defne reduceo
  "Relational reduce, succeds if the reduction produces the result."
  [relo initial coll result]
  ([_ _ () _] (l/== initial result))
  ([_ _ [next-item . remaining] _]
   (l/fresh [result-so-far]
            (relo initial next-item result-so-far)
            (reduceo relo result-so-far remaining result))))

;todo: consider a version that cuts after reaching index
(defn ntho
  "Succeeds if the collection has x as the index n element.
   Classic recursive implementation."
  ([coll index content] (ntho coll index content 0))
  ([coll index content i]
   (l/fresh [head tail]
            (l/firsto coll head)
            (l/resto coll tail)
            (l/conde [(l/== head content) (l/== index i)]
                     [(ntho tail index content (inc i))]))))

(defn lvar-table
  "Creates a 2-dimensional array of logic variables with `m` rows and `n`
   columns. It returns the table and a complete sequence as a pair.
   The main purpose is to hide the irrelevant technical details."
  [m n]
  (let [table (vec (repeatedly m (fn [] (vec (repeatedly n l/lvar)))))
        lvars (apply concat table)]
    [table lvars]))