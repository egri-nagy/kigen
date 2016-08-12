(ns kigen.poset-test
  (:require [clojure.test :refer :all]
            [kigen.poset :as poset]))

(deftest poset_explicit-test
  (testing "Testing implicit to explicit conversion."
    (let [r (poset/rel [1 2 4 8 16] (fn [x y] (= 0 (mod y x))))
          c (poset/cover-rel [1 2 4 8 16] (fn [x y] (= 0 (mod y x))))]
      (is (= {1 #{1}, 2 #{1 2}, 4 #{1 4 2}, 8 #{1 4 2 8}, 16 #{1 4 2 16 8}} r))
      (is (= {1 #{}, 2 #{1}, 4 #{2}, 8 #{4}, 16 #{8}} c)))))
