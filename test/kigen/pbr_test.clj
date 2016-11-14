(ns kigen.pbr-test
  (:require [clojure.test :refer :all]
            [kigen.pbr :as pbr]))

(load-file "resources/arxiv1102.0862.clj")
(load-file "resources/roozbehtest.clj")
(deftest pbr-mul-test
  (testing "Testing pbr multiplication."
    (is (= alphabeta (pbr/mul alpha beta)))
    (is (= RHab (pbr/mul RHa RHb)))))

(deftest pbr-associativity-test
  (testing "Testing pbr associativity."
    (let [a (pbr/rand-pbr 13)
          b (pbr/rand-pbr 13)
          c (pbr/rand-pbr 13)]
      (is (= (pbr/mul (pbr/mul a b) c)
             (pbr/mul a (pbr/mul b c)))))))
