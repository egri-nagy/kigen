(ns kigen.diagram.pbr-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.diagram.pbr :as pbr]))

;from resources/arxiv1102.0862.clj
(def alpha {:dom #{1 2 3 4 5 6 7} :cod #{8 9 10 11 12 13 14 15 16}
            1 #{9} 2 #{} 3 #{4} 4 #{4} 5 #{} 6 #{13} 7 #{14}
            8 #{8} 9 #{1 10} 10 #{11} 11 #{} 12 #{2,12} 13 #{6}
            14 #{7} 15 #{16} 16 #{15}})

(def beta {:dom #{1 2 3 4 5 6 7 8 9} :cod #{10 11 12 13}
           1 #{} 2 #{10} 3 #{3} 4 #{5} 5 #{4 11} 6 #{6} 7 #{} 8 #{12} 9 #{}
           10 #{2} 11 #{} 12 #{8} 13 #{9}})

(def alphabeta {:dom #{1 2 3 4 5 6 7} :cod #{8 9 10 11}
                1 #{8} 2 #{} 3 #{4} 4 #{4} 5 #{} 6 #{6} 7 #{}
                8 #{1 2 9} 9 #{} 10 #{} 11 #{10}})
;from resources/roozbehtest.clj
(def RHa {:dom #{1 2 3 4}
          :cod #{5 6 7 8}
          1 #{} 2 #{7} 3 #{} 4 #{}
          5 #{} 6 #{1,7} 7 #{} 8 #{7,4}})

(def RHb {:dom #{1 2 3 4}
          :cod #{5 6 7 8}
          1 #{} 2 #{} 3 #{2 4} 4 #{8}
          5 #{2} 6 #{} 7 #{} 8 #{}})

(def RHab {:dom #{1 2 3 4}
           :cod #{5 6 7 8}
           1 #{} 2 #{1 4} 3 #{} 4 #{}
           5 #{1 4} 6 #{} 7 #{} 8 #{}})

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
