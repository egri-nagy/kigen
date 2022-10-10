(ns kigen.greens-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.transf :as t]
            [kigen.sgp :as s]
            [kigen.greens :refer :all]))

(def T3 (s/sgp-by-gens (t/full-ts-gens 3) t/mul))

(deftest test-right-ideal
  (testing "Testing principal right ideals."
    (is (= (principal-right-ideal [1 2 2] T3 t/mul)
           (principal-right-ideal [2 0 0] T3 t/mul)))
    (is (= (principal-right-ideal [1 2 1] T3 t/mul)
           #{[0 1 0] [2 0 2] [1 1 1] [1 2 1] [2 2 2]
             [0 2 0] [2 1 2] [1 0 1] [0 0 0]}))))

(deftest test-left-ideal
  (testing "Testing principal left ideals."
    (is (= (principal-left-ideal [0 2 2] T3 t/mul)
           (principal-left-ideal [2 0 0] T3 t/mul)))
    (is (= (principal-left-ideal [1 2 1] T3 t/mul) 
           #{[1 1 1] [1 2 1] [2 2 2] [2 2 1] [2 1 1]
             [1 1 2] [2 1 2] [1 2 2]}))))