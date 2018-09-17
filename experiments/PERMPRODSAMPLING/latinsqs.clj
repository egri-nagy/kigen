(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

(defn extend-latin-rectangle
  "Extending a partial latin square (latin rectangle) by one more row.
  n - dimension of the rectangle
  rect - the partial latin square represented as a vector"
  [n rect]
  ;(print (/ (count rect) n) " ") (flush)
  (let [tab (into rect (vec (repeatedly n l/lvar)))
        rows (partition n tab)
        cols (apply map vector rows)
        points  (fd/interval 0 (dec n))]
    (first (l/run 1 [q]
             (l/everyg #(fd/in % points) tab)
             (l/everyg fd/distinct rows)
             (l/everyg fd/distinct cols)
             (l/== q tab)))))

(defn all-latin-squares
  "All Latin squares from a partial Latin table."
  [n partialtab]
  (let [size (* n n)
        tab (into partialtab
                  (vec (repeatedly (- size (count partialtab)) l/lvar)))
        rows (partition n tab)
        cols (apply map vector rows)
        points (fd/interval 0 (dec n))]
    (l/run* [q]
      (l/everyg #(fd/in % points) tab)
      (l/everyg fd/distinct rows)
      (l/everyg fd/distinct cols)
      (l/== q tab))))

(defn random-latin-square [n]
  (let [size (* n n)]
    (first
     (drop-while (fn [sol] (not (or (nil? sol)
                                    (= size (count sol)))))
                 (iterate (partial extend-latin-rectangle n)
                      (rand-perm n))))))
