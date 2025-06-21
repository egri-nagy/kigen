(ns kigen.holonomy.chain-test
  (:require [clojure.test :refer :all]
            [kigen.holonomy.chain :as chain]))

(deftest chains-test
  (testing "Testing chains in cover relation."
    (let [r {1 #{2 3 4}, 2 #{6}, 3 #{5}, 4 #{5}, 5 #{6}}]
      (is (= (chain/chains 1 5 r)
             #{[1 3 5], [1 4 5]}))
      (is (= #{[1 2]} (chain/chains 1 2 r)))
      (is (= #{[1 4 5] [1 3 5]} (chain/chains 1 5 r)))
      (is (= 3 (count (chain/chains 1 6 r))))
      (is (zero? (count (chain/chains 3 4 r))))
      (is (= 1 (count (chain/chains 3 5 r)))))))
