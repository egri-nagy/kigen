(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[kigen.transf :as transf])

(defn latin-squares [n]
  (let [tab (vec (repeatedly (* n n) l/lvar))
        points  (fd/interval 0 (dec n))]
    (l/run* [q]
      (l/everyg #(fd/in % points) tab)
      (fd/distinct tab)
      (l/== q tab))))


(defn rand-perm
  "A random permutation of degree n."
  [n]
  (shuffle (vec (range n))))

(defn product [v]
  (reduce transf/mul v))

(defn sample [n m k]
  (let [rnd-perms (repeatedly m #(rand-perm n))
        k-tuples (combinatorics/selections rnd-perms k)
        num-of-classes (count (into #{} (map product k-tuples)))
        num-of-tuples (Math/pow m k)]
    (/ num-of-classes num-of-tuples)))
