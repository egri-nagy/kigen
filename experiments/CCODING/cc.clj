(require '[clojure.core.logic :as cl])
(require '[clojure.core.logic.fd :as fd])

(defn sumo
  "This goal succeeds if adding up the elements of l is sum."
  [l sum]
  (cl/fresh [head tail sum-of-remaining]
    (cl/conde
     [(cl/== l ()) (cl/== sum 0)]
     [(cl/conso head tail l)
      (fd/+ head sum-of-remaining sum)
      (sumo tail sum-of-remaining)])))

(defn increasingo
  "This goal succeeds if the elements of l are in non-decreasing order."
  [l v]
  (cl/fresh [head tail]
    (cl/conde
     [(cl/== l ())]
     [(cl/conso head tail l)
      (fd/<= v head)
      (increasingo tail head)])))

(defn find-even-sols
  "All even solution candidates."
  []
  (let [p (vec (repeatedly 8 cl/lvar))
        even (apply fd/domain (range 2 35 2))] ; here we give the possible values
    (cl/run*  [q]
      (cl/everyg #(fd/in % even) p)
      (increasingo p 0)
      (sumo p 140) ; sum given here
      (cl/== q p)))) ; length of vector given here

(defn find-odd-sols
  "All odd solution candidates."
  []
  (let [p (vec (repeatedly 8 cl/lvar))
        odd (apply fd/domain (range 1 35 2))] ; here we give the possible values
    (cl/run*  [q]
      (cl/everyg #(fd/in % odd) p)
      (increasingo p 0)
      (sumo p 140) ; sum given here
      (cl/== q p)))) ; length of vector given here

(defn find-max-odd-sols
  "All odd solution candidates containing 35."
  []
  (let [p (vec (repeatedly 7 cl/lvar))
        odd (apply fd/domain (range 1 35 2))] ; here we give the possible values
    (cl/run*  [q]
      (cl/everyg #(fd/in % odd) p)
      (increasingo p 0)
      (sumo p 105) ; sum given here
      (cl/== q p)))) ; length of vector given here


(clojure.pprint/pprint  (str (count (find-odd-sols))
                             (count (find-even-sols))
                             (count (find-max-odd-sols))))
