(ns kigen.canonical-labeling-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.canonical-labeling :refer :all]))

(deftest can-seq-test
  (testing "Testing canonical labeling and from for sequences."
    ;can-lab- gives labeling info, but not the form
    (is (= (can-lab-seq [:a 6 7 :a "hello"])
           [{:a 0, 6 1, 7 2, "hello" 3} 4]))
    ;can- gives the form
    (is (= (can-seq [:a 6 7 :a "hello"])
           [0 1 2 0 3]))
    ;both can-lab- and can- can start from a partial labeling
    (is (= (can-lab-seq [{:a 10} 11] [:a 6 7 :a "hello"])
           [{:a 10, 6 11, 7 12, "hello" 13} 14]))
    (is (= (can-seq [{:a 3} 4] [:a 6 7 :a "hello"])
           [3 4 5 3 6]))))

(deftest can-seq-seq-test
  (testing "Testing canonical labeling and from for sequence sequences."
    ;can-lab- gives labeling info, but not the form
    (is (can-lab-seq-seq [[1 1 0] [2 0 3]])
        [{1 0, 0 1, 2 2, 3 3} 4])
    ;can- gives the form
    (is (= (can-seq-seq [[1 1 0] [2 0 3]])
           [[0 0 1] [2 1 3]]))
    ;both can-lab- and can- can start from a partial labeling 
    (is (= (can-lab-seq-seq [{1 31} 42] [[1 1 0] [2 0 3]])
           [{1 31, 0 42, 2 43, 3 44} 45]))
    (is (= (can-seq-seq [{1 42} 100] [[1 1 0] [2 0 3]])
           [[42 42 100] [101 100 102]]))))

(deftest can-set-seq-test
  (testing "Testing canonical labeling and from for set of sequences."
    ;can-lab- gives labeling info and the form, there can be many labelings
    (is (can-lab-set-seq [[1 0 1] [2 1 1] [1 2 2]])
        '(([{1 0, 0 1, 2 2} 3] [[0 1 0] [0 2 2] [2 0 0]])))
    ;can- gives the form, and there is only one
    (is (= (can-set-seq [[1 0 1] [2 1 1] [1 2 2]])
           [[0 1 0] [0 2 2] [2 0 0]]))
    ;worse case scenario, we get the symmetric group! 
    (is (= (can-lab-set-seq [[2 2 2] [1 1 1] [0 0 0]])
           '(([{2 0, 1 1, 0 2} 3] [[0 0 0] [1 1 1] [2 2 2]])
             ([{2 0, 0 1, 1 2} 3] [[0 0 0] [1 1 1] [2 2 2]])
             ([{1 0, 2 1, 0 2} 3] [[0 0 0] [1 1 1] [2 2 2]])
             ([{1 0, 0 1, 2 2} 3] [[0 0 0] [1 1 1] [2 2 2]])
             ([{0 0, 2 1, 1 2} 3] [[0 0 0] [1 1 1] [2 2 2]])
             ([{0 0, 1 1, 2 2} 3] [[0 0 0] [1 1 1] [2 2 2]]))))))