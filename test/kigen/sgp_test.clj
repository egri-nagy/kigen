(ns kigen.sgp-test
  (:require [clojure.test :refer :all]
            [kigen.transf :as t]
            [kigen.sgp :as sgp]))

(deftest commutativity-test
  (testing "Testing commutative?"
    (is (= true (sgp/commutative? (t/sgp-by-gens (t/cyclic-gens 11))
                                  t/mul)))
    (is (= false (sgp/commutative? (t/sgp-by-gens (t/full-ts-gens 4))
                                   t/mul)))))

(deftest commutativity-test
  (testing "Testing index-period."
    (is (= [4 1] (sgp/index-period [1 1 3 0 2] t/mul)))
    (is (= [1 1] (sgp/index-period [0  1 2 3 4 5 6 7] t/mul)))))
