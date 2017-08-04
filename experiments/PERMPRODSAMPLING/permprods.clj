(use '[clojure.math.combinatorics :as combinatorics])
(use '[kigen.transf :as transf])

(defn rand-perm
  "A random permutation of degree n."
  [n]
  (shuffle (vec (range n))))

(defn sample [n m k]
  (count (into #{}
               (map #(reduce transf/mul %)
                    (combinatorics/selections (repeatedly m #(rand-perm n))
                                              k)))))
