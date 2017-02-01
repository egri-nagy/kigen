(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms")

;just for experimenting with
(def T2 [ [ 0, 0, 3, 3 ],
         [ 0, 1, 2, 3 ],
         [ 0, 2, 1, 3 ],
         [ 0, 3, 0, 3 ] ])

(defmacro at [mt i j]
    `((~mt ~i) ~j))

(defn morphic?
  ([S T hom] (morphic? S T hom (vec (range (count hom))) (set hom))) ; TODO reverse here maybe?
  ([S T hom, dom, cod]
   (letfn [(f [x y] (let [z (at S x y)
                          t (at T (hom x) (hom y))]
                      (or (and (contains? cod t) ; verbose logic, should be reduced
                               (contains? dom z)
                               (= t (hom z)))
                          (and (not (contains? cod t))
                               (not (contains? dom z))))))]
     (nil? (first (for [x dom
                        y dom
                        :when (not (f x y))]
                    [x y]))))))

(defn backtrack
  "S source set of elements, T target set of elements,
  hom - (partial) morphism
  morphic? - takes a partial morphism and decides
  choices - the possible next elements based on partial solution and source"
  [S,T,hom,  morphic?, choices]
  (if (= (count S) (count hom))
    hom ; it is a solution
    (let [morphicones (filter #(morphic? hom %) (choices hom S))
          ]
      nil)))
