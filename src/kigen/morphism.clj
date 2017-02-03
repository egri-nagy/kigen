(ns kigen.morphism
  "Constructing morphisms and morphic relations.
  input: two multiplication tables (source, target)
  output: vectors describing morphisms")


(defmacro at [mt i j]
    `((~mt ~i) ~j))

(defn morphic?
  "Decides whether the mapping hom from S to T is homomorphic or not."
  [S T hom]
  (let [dom (vec (range (count hom)))
        cod (set hom)]
    (letfn [(f [x y] (let [z (at S x y)
                           XY (at T (hom x) (hom y))]
                       (or
                        ;both defined but not matching
                        (and (contains? cod XY)
                                (contains? dom z)
                                (not= XY (hom z)))
                        ;finishing but XY not defined
                        (and (= (count dom) (count S))
                                (not (contains? cod XY))))))]
      (nil? (first (for [x dom y dom :when (f x y)] [x y]))))))

(defn search
  "S source set of elements, T target set of elements,
  hom - (partial) morphism
  morphic? - takes a partial morphism and decides
  choices - the possible next elements based on partial solution and source"
  [S,T,hom,  morphic?, choices]
  (loop [homs [hom]]
    (if (= (count (first homs)) (count S))
      homs
      (recur (mapcat
              (fn [hom] (filter
                         #(morphic? S T %)
                         (map #(conj hom %) choices)))
              homs)))))

(defn morphisms
  "S source multab
   T target multab
  hom - (partial) morphism
  morphic? - predicate deciding whether a map is a (potential) morphism or not
  choices - the possible next elements based on partial solution and source"
  [S,T,hom,  morphic?]
  (let [Tset (set (range (count T)))]
    (letfn [(backtrack  [hom]
              (if (= (count hom) (count S))
                [hom]
                (let [extensions (filter
                                  #(morphic? S T %)
                                  (map #(conj hom %) Tset))] 
                  (mapcat backtrack  extensions))))]
      (backtrack hom))))
