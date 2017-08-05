(use '[clojure.math.combinatorics :as combinatorics])
(use '[kigen.transf :as transf])

(defn rand-perm
  "A random permutation of degree n."
  [n]
  (shuffle (vec (range n))))

(defn product [v]
  (reduce transf/mul v))

(defn sample [n m k]
  (let [rnd-perms (repeatedly m #(rand-perm n))
        k-tuples (combinatorics/selections rnd-perms k)
        num-of-classes (count (into #{} (map product k-tuples)))]
    (/ num-of-classes (Math/pow m k))))
