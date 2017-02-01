(ns kigen.backtrack
  "Partitioned backtrack search for morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms")

;just for experimenting with
(def T2 [ [ 0, 0, 3, 3 ],
         [ 0, 1, 2, 3 ],
         [ 0, 2, 1, 3 ],
         [ 0, 3, 0, 3 ] ])

(defmacro at [mt i j]
    `((~mt ~i) ~j))

(defn good?
  ([S T hom] (good? S T hom (range (count S)) (set hom)))
  ([S T hom, dom, cod]
   (let [dom (range (count S))]
     (nil? (first (for [x dom
                        y dom
                        :when (not= (hom (at S x y))
                                    (at T (hom x) (hom y)))]
                    [x y]))))))

(defn backtrack
  "S source set of elements, T target set of elements,
  hom - (partial) morphism
  good? - takes a partial solution and the next element
  choices - the possible next elements based on partial solution and source"
  [S,T,hom,  good?, choices]
  (if (= (count S) (count hom))
    hom ; it is a solution
    (let [goodones (filter #(good? hom %) (choices hom S))
          ]
      nil)))
