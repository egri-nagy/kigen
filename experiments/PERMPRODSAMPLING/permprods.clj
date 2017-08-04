(defn rand-perm
  "A random permutation of degree n."
  [n]
  (shuffle (vec (range n))))


