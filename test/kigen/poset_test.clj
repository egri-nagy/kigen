(ns kigen.poset-test
  (:require [clojure.test :refer :all]
            [kigen.poset :as poset]))

(deftest poset-explicit-test
  (testing "Testing implicit to explicit conversion."
    (let [r? (fn [x y] (= 0 (mod y x)))
          r (poset/rel [1 2 4 8 16] r? )
          c (poset/cover-rel [1 2 4 8 16] r?)]
      (is (= r {1 #{1 2 4 8 16}, 2 #{2 4 8 16}, 4 #{4 8 16}, 8 #{8 16},
                16 #{16}}))
      (is (= c {1 #{2}, 2 #{4}, 4 #{8}, 8 #{16}, 16 #{}} )))))

(deftest poset-chain-test
  (testing "Testing chains in cover relation."
    (let [r {1 #{2 3 4}, 2 #{6}, 3 #{5}, 4 #{5}, 5 #{6}}]
      (is (=
           (set (poset/chains 1 5 r))
           #{[1 3 5], [1 4 5]} )))))
