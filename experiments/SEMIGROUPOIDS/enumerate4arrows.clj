;; finding all semigroupoids with up to 4 arrows
;; v25.06.xx
(require '[kigen.semigroupoid.enumeration :refer [semigroupoids-order-n]])
(require '[kigen.semigroupoid.homomorphism :refer [comptabs-up-to-morphisms]])
(require '[clojure.math.combinatorics :refer [selections]])

;(doseq [sgpoid (comptabs-up-to-morphisms (semigroupoids-order-n 3))]
;  (println sgpoid))

;(println (count (semigroupoids-order-n 3)))

(def T '[4 - - -
         - 0 - -
         - - 1 -
         - - - 2])

(def diagonals (selections (range 5) 4))

(def diagonal-positions
  (map (fn [i] (+ i (* 4 i))) (range 4)))

(defn produce-tables
  [n positions diagonals]
  (let [m (zipmap positions diagonals)]
    (mapv
     (fn [i]
       (m i '-))
     (range (* n n)))))

;test
(produce-tables 4 [0 5 10 15 ] [0 1 2 3])

(doseq [diagonal diagonals]
  (println ";" diagonal)
  (doseq [sgpoid (semigroupoids-order-n
                  4
                  (produce-tables 4 diagonal-positions diagonal))]
    (println sgpoid)))
