(ns kigen.digraph.isomorphism-test
  (:require [kigen.digraph.isomorphism
             :refer [digraph-isomorphisms
                     digraphs-up-to-morphisms]]
            [clojure.test
             :refer [deftest
                     testing
                     is]]))

(deftest digraph-isomorphisms-test
  (testing "Testing Digraph isomorphisms"
    (let [G [[0 1] [1 2] [2 3] [3 0]]
          H [[0 1] [1 2] [2 3] [3 3]]]
      (is (=  #{[0 1 2 3] [1 2 3 0] [2 3 0 1] [3 0 1 2]}
              (set (digraph-isomorphisms G G))
              (set (digraph-isomorphisms (set G) (set G)))))
      (is (empty? (digraph-isomorphisms G H))))))
