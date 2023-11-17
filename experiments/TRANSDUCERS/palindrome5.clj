(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transducer.viz :refer [DotTransducer Dot2PDF]])

;; levels: :warn, :info, :debug
(timbre/set-min-level! :info)

;; PALINDROMES
(defn palindromes
  [n]
  (mapv (fn [l]
          [(vec l)
           (if (= l (reverse l))
             :palindrome
             :ordinary)])
        (combo/selections [0 1] n)))

(def plndrm3 (palindromes 3))
(experiment "palindromes 3 flexible" plndrm3 4 (comp first f/transducer))

(def plndrm4 (palindromes 4))
(experiment "palindromes 4 flexible" plndrm4 5 (comp first f/transducer))

(def plndrm5 (palindromes 5))
(experiment "palindromes 5 flexible" plndrm5 6 (comp first f/transducer))

(experiment "palindromes 5 flexible" plndrm5 8 (comp first f/transducer))
