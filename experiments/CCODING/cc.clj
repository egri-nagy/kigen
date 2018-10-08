(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])



(defn f
  ""
  [n]

  (let [tup (vec (repeatedly n l/lvar))
        points  (fd/interval 0 (dec n))]
    (l/run* [q]
      (l/everyg #(fd/in % points) tup)
      (fd/<= (tup 0) (tup 1))
      (fd/<= (tup 1) (tup 2))
      (fd/+ (tup 0) (tup 1) 2)
      (l/== q tup))))
