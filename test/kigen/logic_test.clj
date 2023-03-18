(ns kigen.logic-test
  (:require [clojure.test :refer :all]
            [kigen.logic :as kl]
            [clojure.core.logic :as l] 
            [clojure.core.logic.fd :as fd]))

(deftest reduceo-test
  (testing "Testing reduceo." 
    (is (= [4] (l/run 1 [q] (kl/reduceo fd/+ 0 [1 2 3 q] 10))))
    (is (= [10] (l/run 1 [q] (kl/reduceo fd/+ 0 [1 2 3 4] q))))
    (is (= [0] (l/run 1 [q] (kl/reduceo fd/+ q [1 2 3 4] 10))))
    
    (is (= [1] (l/run 1 [q]
                  (kl/reduceo fd/* q [1 2 3 4] 24)
                  (kl/reduceo fd/+ q [2 1 1 1] 6))))
    (is (= '(_0) (l/run 1 [q] (kl/reduceo l/conjo [] [1 2 3] [1 2 3]))))
    (is (= [2] (l/run 1 [q] (kl/reduceo l/conjo [] [1 2 3] [1 q 3]))))
    (is (= [[1 2 3]] (l/run 1 [q] (kl/reduceo l/conjo [] [1 2 3] q)) ))
    ;BUT!
    (is (= []  (l/run 1 [q] (kl/reduceo l/conjo q [1 2 3] [1 2 3]))))))