;; finding all semigroupoids with up to 4 arrows
;; v25.06.xx
(require '[kigen.semigroupoid.enumeration :refer [semigroupoids-order-n]])
(require '[kigen.semigroupoid.homomorphism :refer [comptabs-up-to-morphisms]])
(require '[clojure.math.combinatorics :refer [selections]])

;all possible 4-tuples of the five values
(def diagonals (selections (range 5) 4))

;computing the coordinates for the diagonal entries
(def diagonal-positions
  (map (fn [i] (+ i (* 4 i))) (range 4)))

;creating the template composition table
(defn produce-table
  [n positions diagonals]
  (let [m (zipmap positions diagonals)]
    (mapv
     (fn [i]
       (m i '-))
     (range (* n n)))))

;test
(produce-table 4 [0 5 10 15 ] [0 1 2 3])

(println "(def n4solutions [")
(doseq [diagonal diagonals]
  (println ";" diagonal)
  (doseq [sgpoid (semigroupoids-order-n
                  4
                  (produce-table 4 diagonal-positions diagonal))]
    (println sgpoid)))
(println "])")
