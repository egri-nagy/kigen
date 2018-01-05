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
