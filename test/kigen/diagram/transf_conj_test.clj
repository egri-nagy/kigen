(ns kigen.diagram.transf-conj-test
  (:require [clojure.test :refer :all]
            [kigen.sgp :refer [sgp-by-gens]]
            [kigen.diagram.transf :as t]
            [kigen.diagram.transf-conj :as t-c]
            [kigen.conjugacy :as conjugacy]))

;;CONJUGATION
(deftest realize-a-mapping-test
  (testing "Realizing an individual map."
    (is (= {1 3, 2 4} (t-c/realize-a-mapping [1 2] [3 4] {})))
    (is (= {2 3, 1 4} (t-c/realize-a-mapping [2 1] [3 4] {})))
    (is (= nil (t-c/realize-a-mapping [1 2] [3 4] {1 2})))
    (is (= {1 2, 2 1} (t-c/realize-a-mapping [1 2] [2 1] {})))
    (is (= {1 1, 2 4} (t-c/realize-a-mapping [1 2] [1 4] {})))
    (is (= {1 3, 2 2} (t-c/realize-a-mapping [1 2] [3 2] {})))
    (is (= nil (t-c/realize-a-mapping [1 2] [3 2] {2 3})))
    (is (= {1 2, 2 3, 3 4, 4 5,5 1}
           (t-c/realize-a-mapping [1 2 3 4 5] [2 3 4 5 1] {})))
    ))

(deftest transf-conjugate-test
  (testing "Conjugates."
    (is (= [4 3 0 3 1] (t/conjugate [1 4 3 3 2] [2 0 1 3 4])))
    (is (= [4 3 0 3 1] (t/conjugate-by-definition [1 4 3 3 2] [2 0 1 3 4])))))

(deftest transf-conjrep-test
  (testing "Conjugacy class representatives, direct search vs. naive method."
    (let [S5 (sgp-by-gens (t/symmetric-gens 5) t/mul)
          T5 (sgp-by-gens (t/full-ts-gens 5) t/mul)
          sets [
                [[1 2 1 2 1] [3 3 1 2 0] [4 1 3 0 2]]
                [[3 3 1 2 0] [4 1 3 0 2]]
                [[1 2 1 2 1] [0 0 1 2 0] [4 1 3 0 2] [1 2 1 2 1] [0 1 0 1 0]]
                ]]
      (is (every? #(and
                    (t-c/conjrep %)
                    (conjugacy/conjrep-by-minimum t/conjugate % S5))
                  T5))
      (is (every? #(= (t-c/setconjrep %)
                      (conjugacy/setconjrep t/conjugate % S5))
           sets)) )))

(deftest transf-test-conjugators
  (testing "Minimal conjugators."
    (let [T4 (sgp-by-gens (t/full-ts-gens 4) t/mul)
          S4 (sgp-by-gens (t/symmetric-gens 4) t/mul)]
      (is (empty?
           (filter
            (fn [x] (not= (set (t-c/conjugators x (t-c/conjrep x)))
                          (set (second
                                (conjugacy/minconjugators
                                 t/conjugate
                                 x
                                 S4)))))
            T4))))))
