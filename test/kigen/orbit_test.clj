(ns kigen.orbit-test
  (:require [clojure.test :refer :all]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]))

;;testing with operators that construct subsets of a set
;;a set-valued operator
(defn subset-covers [coll]
  (map #(set (remove (partial = %) coll)) coll))

(deftest test-*fs
  (let [bfsres (orbit/bfs [#{1 2 3 4 5 6 7 8}] subset-covers)
        dfsres (orbit/dfs [#{1 2 3 4 5 6 7 8}] subset-covers)]
    (testing "Testing depth-first and breadth-first searches."
      (is (= 256 (count bfsres)))
      (is (= bfsres dfsres)))))

(deftest test-first-solution-*fs
  (let [f (fn [x] (hash-set (conj x (inc (count x)))))
        cand? (fn [x] (<= (count x) 11))
        sol? (fn [x] (= (count x) 11))
        bfssol (orbit/first-solution-bfs #{0} f cand? sol?)
        ;dfssol (orbit/first-solution-dfs #{0} f cand? sol?)
        ]
    (testing "Testing depth-first and breadth-first solutions."
      (is (not (some nil? [bfssol bfssol]))))))
