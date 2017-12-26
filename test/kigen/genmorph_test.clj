(ns kigen.genmorph-test
  (:require [clojure.test :refer :all]
            [kigen.genmorph :refer :all]
            [kigen.transf :as transf]))

(deftest test-multab
  (testing "Testing embeddings by generators."
    (let [T2gens (transf/full-ts-gens 2)
          T3gens (transf/full-ts-gens 3)
          T4gens (transf/full-ts-gens 4)
          S3 (transf/sgp-by-gens (transf/symmetric-gens 3))
          S4 (transf/sgp-by-gens (transf/symmetric-gens 4))
          tmul transf/mul
          tconj transf/conjugate]
      (is (= 6 (count
                (sgp-embeddings-by-gens T3gens tmul
                                        T3gens tmul ))))
      (is (= 1 (count
                (sgp-embeddings-by-gens T3gens tmul
                                        T3gens tmul
                                        tconj S3))))
      (is (= 3 (count
                (sgp-embeddings-by-gens T2gens tmul
                                        T3gens tmul
                                        tconj S3))))
      (is (= 12 (count
                (sgp-embeddings-by-gens T2gens tmul
                                        T4gens tmul
                                        tconj S4))))
      (is (= 4 (count
                (sgp-embeddings-by-gens T3gens tmul
                                        T4gens tmul
                                        tconj S4)))))))
      
