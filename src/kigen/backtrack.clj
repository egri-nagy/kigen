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

(defn good? [S, T, hom, t]
  (let [dom (range (count S))]
    (every?
     (fn [x] (every?
              (fn [y] (= (hom (at S x y))
                         (at T (hom x) (hom y))))
              dom))
     dom)))

(defn backtrack
  "S source set of elements, T target set of elements,
  psol - partial solution
  good? - takes a partial solution and the next element
  choices - the possible next elements based on partial solution and source"
  [S,T,psol,  good?, choices]
  (if (= (count S) (count psol))
    psol ; it is a solution
    (let [goodones (filter #(good? psol %) (choices psol S))
          ]
      nil)))
