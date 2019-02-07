(require '[clojure.core.logic :as cl])
(require '[clojure.core.logic.fd :as fd])

(defn everyo
  "This goal succeeds if every element of l succeeds for f."
  [l f]
  (cl/fresh [head tail]
    (cl/conde
     [(cl/== l ())]
     [(cl/conso head tail l)
      (f head)
      (everyo tail f)])))

(defn sumo
  "This goal succeeds if adding up the elements of l is sum."
  [l sum]
  (cl/fresh [head tail sum-of-remaining]
    (cl/conde
     [(cl/== l ()) (cl/== sum 0)]
     [(cl/conso head tail l)
      (fd/+ head sum-of-remaining sum)
      (sumo tail sum-of-remaining)])))

(defn counto
  "This goal succeeds if l has s number of elements."
  [l s]
  (cl/fresh [head tail remaining]
    (cl/conde
     [(cl/== l ()) (cl/== s 0)]
     [(cl/conso head tail l)
      (fd/+ 1 remaining s)
      (counto tail remaining)])))

(defn increasingo
  "This goal succeeds if the elements of l are in non-decreasing order."
  [l v]
  (cl/fresh [head tail]
    (cl/conde
     [(cl/== l ())]
     [(cl/conso head tail l)
      (fd/<= v head)
      (increasingo tail head)])))


(defn find-sols
  "The main function for finding solutions."
  []
  (let [even (apply fd/domain (range 2 35 2))
        odd (apply fd/domain (range 1 35 2))] ; here we give the possible values
    (cl/run 10 [q] ; the number of expected solutions (if more given, then it hangs)
      (cl/conde
       [(everyo q #(fd/in % even))]
       [(everyo q #(fd/in % odd))])
      (increasingo q 0)
      (sumo q 140) ; sum given here
      (counto q 8 )))) ; length of vector given here

(println
 (find-sols))
