(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[kigen.transf :as transf])

(defn rand-perm
  "A random permutation of degree n."
  [n]
  (shuffle (vec (range n))))

(defn extend-latin-rectangle [n rect]
  (print (/ (count rect) n) " ") (flush)
  (let [tab (into rect (vec (repeatedly n l/lvar)))
        rows (partition n tab)
        cols (apply map vector rows)
        points  (fd/interval 0 (dec n))]
    (first (l/run 1 [q]
             (l/everyg #(fd/in % points) tab)
             (l/everyg fd/distinct rows)
             (l/everyg fd/distinct cols)
             (l/== q tab)))))

(defn random-latin-square [n]
  (let [size (* n n)]
    (first (drop-while (fn [sol] (not (or (nil? sol)
                                          (= size (count sol)))))
             (iterate (partial extend-latin-rectangle n)
                      (rand-perm n))))))

(defn product [v]
  (reduce transf/mul v))

(defn sample [n m k]
  (let [rnd-perms (repeatedly m #(rand-perm n))
        k-tuples (combinatorics/selections rnd-perms k)
        num-of-classes (count (into #{} (map product k-tuples)))
        num-of-tuples (Math/pow m k)]
    (/ num-of-classes num-of-tuples)))

(dotimes [n 1000]  (spit (str (gensym)) (random-latin-square 16)))
