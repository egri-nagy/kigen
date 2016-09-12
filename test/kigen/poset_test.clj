(ns kigen.poset-test
  (:require [clojure.test :refer :all]
            [kigen.poset :as poset]))

(deftest poset-explicit-test
  (testing "Testing implicit to explicit conversion."
    (let [r? (fn [x y] (= 0 (mod y x)))
          r (poset/rel [1 2 4 8 16] r?)
          c (poset/cover-rel [1 2 4 8 16] r?)]
      (is (= r {1 #{1 2 4 8 16}, 2 #{2 4 8 16}, 4 #{4 8 16}, 8 #{8 16},
                16 #{16}}))
      (is (= c {1 #{2}, 2 #{4}, 4 #{8}, 8 #{16}, 16 #{}})))))
