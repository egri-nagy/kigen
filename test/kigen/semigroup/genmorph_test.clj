(ns kigen.semigroup.genmorph-test
  (:require [clojure.test :refer :all]
            [kigen.semigroup.genmorph :refer :all]
            [kigen.semigroup.sgp :refer [sgp-by-gens]]
            [kigen.diagram.transf :as transf]
            [kigen.semigroup.conjugacy :as c]))

;(deftest gentab)

(deftest test-multab
  (testing "Testing embeddings by generators."
    (let [T1gens (transf/full-ts-gens 1)
          T2gens (transf/full-ts-gens 2)
          T3gens (transf/full-ts-gens 3)
          T4gens (transf/full-ts-gens 4)
          S3 (sgp-by-gens (transf/symmetric-gens 3) transf/mul)
          S4 (sgp-by-gens (transf/symmetric-gens 4) transf/mul)
          tmul transf/mul
          tconj transf/conjugate]
      (is (= 6 (count
                (sgp-embeddings-by-gens T3gens tmul
                                        T3gens tmul ))))
      (is (= 1 (count
                (sgp-embeddings-by-gens T3gens tmul
                                        T3gens tmul
                                        (c/conjugation-fn-bundle tconj S3)))))
      (is (= 3 (count
                (sgp-embeddings-by-gens T2gens tmul
                                        T3gens tmul
                                        (c/conjugation-fn-bundle tconj S3)))))
      (is (= 12 (count
                (sgp-embeddings-by-gens T2gens tmul
                                        T4gens tmul
                                        (c/conjugation-fn-bundle tconj S4)))))
      (is (= 4 (count
                (sgp-embeddings-by-gens T3gens tmul
                                        T4gens tmul
                                        (c/conjugation-fn-bundle tconj S4)))))
      (is (= 5 (count
                (sgp-embeddings-by-gens T1gens tmul
                                        T4gens tmul
                                        (c/conjugation-fn-bundle tconj
                                                                 S4))))))))
