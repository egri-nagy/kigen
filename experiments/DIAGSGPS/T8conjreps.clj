;; Finding all conjugacy class representative transformations of the full
;; transformation semigroup Tn.
;; Exploiting the fact that the maximal conjrep is of the form
;; [1 2 3 .. n-1 0]
;; (for the cyclic permutation of n points) we enumerate all transformations
;; lexicographically, odometer style.
;; recreating integer sequence A001372

(require '[kigen.transf :as t])

;; m is the maximal available digit, e.g. 1 for binary, 9 for decimal
(defn nxt
  "Returns the next vector in lexicographic ordering.
  Leftmost is the most significant digit.
  Replacing the the first maximal digits with 0s, increase the 1st of
  the remaining digits, then copy leftover."
  [v m]
  (let [[ms others] (split-with (partial = m) v)]
    (vec (concat (repeat (count ms) 0)
                 (when-not (empty? others)
                   (concat [(inc (first others))]
                           (rest others)))))))

(defn zerovec
  "A vector containing m+1 zeroes."
  [m]
  (vec (repeat (inc m) 0)))

(defn maxcnj
  "Maximal conjugacy class representative for the full transformation semigroup
  of degree m+1."
  [m]
  (vec (concat (range 1 (inc m)) [0])))

(defn full-Tm+1-conjreps
  [m]
  (let [reps (reduce
              (fn [coll v]
                (conj coll ((comp t/conjrep reverse) v)))
              #{}
              (take-while #(not= (vec  (reverse %)) (maxcnj m))
                          (iterate #(nxt % m) (zerovec m))))]
    (conj reps (maxcnj m)))) ; not to forget the maximal conjrep

;; checking:
(println (map (comp count full-Tm+1-conjreps) [0 1 2 3 4 5]))
;; should produce
;; (1 3 7 19 47 130)
;; in  a couple of seconds

;; THE REAL CALCULATION - about an hour
;;(spit "experiments/DIAGSGPS/T8conjreps" (vec (sort (full-Tm+1-conjreps 7))))

;; this can be slurped back by
;; (def T8conjreps (clojure.edn/read-string
;;                   (slurp "experiments/DIAGSGPS/T8conjreps")))

;; checked against data from
;; http://www-groups.mcs.st-andrews.ac.uk/~jamesm/data.php
;; 2017.03.22.
