(ns kigen.backtrack
  "Partitioned backtrack search for morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms")

(defn backtrack
  "S source set of elements, T target set of elements,
  good? - takes a partial solution and the next element
  choices - function returning the possible next elements based on partial solution and source" 
  [S,T, good?, choices]

  )

