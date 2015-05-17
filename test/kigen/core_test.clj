(ns kigen.core-test
  (:require [clojure.test :refer :all]
            [kigen.core :refer :all]))

(deftest sanity-test
  (testing "Testing life, the universe and everything."
    (is (= 42 (* 6 7)))))
