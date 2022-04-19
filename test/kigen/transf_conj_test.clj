(ns kigen.transf-conj-test
  (:require [clojure.test :refer :all]
            [kigen.transf :as t]
            [kigen.transf-conj :as t-c]
            [kigen.sgp :as sgp]
            [kigen.conjugacy :as conjugacy]))

;;CONJUGATION
(deftest realize-a-mapping-test
  (testing "Realizing an individual map."
    (is (= {1 3, 2 4} (t-c/realize-a-mapping [1 2] [3 4] {})))
    (is (= nil (t-c/realize-a-mapping [1 2] [3 4] {1 2})))
    (is (= {1 2, 2 1} (t-c/realize-a-mapping [1 2] [2 1] {})))
    (is (= {1 1, 2 4} (t-c/realize-a-mapping [1 2] [1 4] {})))
    (is (= {1 3, 2 2} (t-c/realize-a-mapping [1 2] [3 2] {})))
    (is (= nil (t-c/realize-a-mapping [1 2] [3 2] {2 3})))
    ))

(deftest transf-conjugate-test
  (testing "Conjugates."
    (is (= [4 3 0 3 1] (t/conjugate [1 4 3 3 2] [2 0 1 3 4])))
    (is (= [4 3 0 3 1] (t/conjugate-by-definition [1 4 3 3 2] [2 0 1 3 4])))))

(deftest transf-conjrep-test
  (testing "Conjugacy class representatives, direct search vs. naive method."
    (let [S5 (t/sgp-by-gens (t/symmetric-gens 5))
          T5 (t/sgp-by-gens (t/full-ts-gens 5))
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
    (let [T4 (t/sgp-by-gens (t/full-ts-gens 4))
          S4 (t/sgp-by-gens (t/symmetric-gens 4))]
      (is (empty?
           (filter
            (fn [x] (not= (set (t-c/conjugators x (t-c/conjrep x)))
                          (set (second
                                (conjugacy/minconjugators
                                 t/conjugate
                                 x
                                 S4)))))
            T4))))))
