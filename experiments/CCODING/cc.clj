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
  (cl/fresh [a d sum-of-remaining]
    (cl/conde
     [(cl/== l ()) (cl/== sum 0)]
     [(cl/conso a d l)
      (fd/+ a sum-of-remaining sum)
      (sumo d sum-of-remaining)])))

(defn counto
  "This goal succeeds if l has s number of elements."
  [l s]
  (cl/fresh [head tail remaining]
    (cl/conde
     [(cl/== l ()) (cl/== s 0)]
     [(cl/conso head tail l)
      (fd/+ 1 remaining s)
      (counto tail remaining)])))

(defn find-sols
  "The main function for finding solutions."
  []
  (let [domain (fd/interval 0 7)] ; here we give the possible values
    (cl/run 5 [q] ; the number of expected solutions (if more given, then it hangs)
      (everyo q #(fd/in % domain))
      (sumo q 10) ; sum given here
      (counto q 2)))) ; length of vector given here

(println
 (find-sols))
