(require '[clojure.math.combinatorics :as combinatorics])
(require '[kigen.transf :as transf])

;; uses java.util.Collections/shuffle
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

                                        ;(defn uuid [] (str (java.util.UUID/randomUUID)))

                                        ;(spit (uuid) (random-latin-square 16))
