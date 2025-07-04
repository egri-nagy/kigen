(ns kigen.table.multab-morphism-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.table.multab-morphism :refer [relmorphisms
                                                 divisions
                                                 isomorphisms
                                                 homomorphisms]]
            [kigen.semigroup.sgp :refer [sgp-by-gens]]
            [kigen.table.multab :as mt]
            [kigen.diagram.transf :as transf]))

(deftest test-multab
  (testing "Testing morphisms by multiplication tables."
    (let [mtT2 (mt/multab (sgp-by-gens (transf/full-ts-gens 2) transf/mul)
                          transf/mul)
          mtS3 (mt/multab (sgp-by-gens (transf/symmetric-gens 3) transf/mul)
                          transf/mul)]
      ;T2 -> T2
      (is (= 120 (count (relmorphisms mtT2 mtT2))))
      (is (= 2 (count (divisions mtT2 mtT2))))
      (is (= 2 (count (isomorphisms mtT2 mtT2))))
      (is (= 7 (count (homomorphisms mtT2 mtT2))))
      ;T2-> S3
      (is (= 22 (count (relmorphisms mtT2 mtS3))))
      (is (= 0 (count (divisions mtT2 mtS3))))
      (is (= 0 (count (isomorphisms mtT2 mtS3))))
      (is (= 1 (count (homomorphisms mtT2 mtS3))))
      ;S3 -> S3
      (is (= 16 (count (relmorphisms mtS3 mtS3))))
      (is (= 6 (count (divisions mtS3 mtS3))))
      (is (= 6 (count (isomorphisms mtS3 mtS3))))
      (is (= 10 (count (homomorphisms mtS3 mtS3)))))))
