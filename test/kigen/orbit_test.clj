(ns kigen.orbit-test
  (:require [clojure.test :refer :all]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]))

;;testing with operators that construct subsets of a set
;;a set-valued operator
(defn subset-covers [coll]
  (map #(set (remove (partial = %) coll)) coll))

(deftest test-sdfs
  (testing "Testing depth-first search with set-valued operator."
    (is (= 32 (count (orbit/sdfs [#{1 2 3 4 5}] subset-covers))))))

(comment  (deftest test-*fs
            (let [ops (for [x [1 2 3 4 5]] #(set (remove (partial = x ) %)))
                  bfsres (orbit/bfs [#{1 2 3 4 5}] ops)
                  dfsres (orbit/dfs [#{1 2 3 4 5}] ops)
                  dfs2res (orbit/dfs2 [#{1 2 3 4 5}] ops)]
              (testing "Testing depth-first and breadth-first searches."
                (is (= 32 (count bfsres)))
                (is (= bfsres dfsres dfs2res))))))
