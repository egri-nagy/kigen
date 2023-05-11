; trajectory based transducer construction
; this way we can naturally do partial automata, but it does not
; scale well due to the large number of lvars (total sum of length of
; all trajectories)

(require '[kigen.logic :as kl])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug set-min-level! *config* merge-config!]])
(require '[kigen.position :refer [index]])
(require '[clojure.pprint :refer [pprint]])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.from-trajectories :refer [transducer]])

; levels: :warn, :info, :debug
(set-min-level! :info)
(defn simple-logger
  [m]
  (str (:min-level (:config m)) " "
       (:vargs m)))
(merge-config! {:output-fn simple-logger})


;; (l/run*
;;  [q]
;;  (compatiblo [0 0] [1 0])
;;  (compatiblo [0 0] [1 1]))

;; (l/run* [q] (compatiblo [0 0] [0 1]))
;; (l/run* [q] (compatiblo [2 0] [2 1]))

;; (l/run* [q]
;;         (l/membero q [0 1 2])
;;         (compatiblo [1 2] [q 2]))

;; (l/run* [q]
;;         (l/membero q [0 1 2])
;;         (compatiblo [1 2] [1 q]))

;; (let [X (range 3)]
;;   (l/run* [q] (l/fresh [a b]
;;                        (l/== q [a b])
;;                        (l/membero a X)
;;                        (l/membero b X)
;;                        (compatiblo [:x 0] q))))

;; (let [X (range 3)]
;;   (l/run* [q] (l/fresh [a b]
;;                        (l/== q [a b])
;;                        (l/membero a X)
;;                        (l/membero b X)
;;                        (compatiblo [1 0] q))))




;; (l/run* [q] (compatible-with-collo [0 0] [[0 0] [1 0] [1 1]]))
;; ; collection itself can be not compatible
;; (l/run* [q] (compatible-with-collo [0 0] [[1 0] [1 1]]))

;; (l/run* [q] (compatible-with-collo [0 0] [[2 1] [1 0] [0 1]]))


;; (l/run* [q]
;;        (l/== q [[(l/lvar) (l/lvar)] [(l/lvar) (l/lvar)]])
;;        (compatible-collo q))
;; ;todo, interpret this
;; (l/run* [q]
;;         (l/fresh [a b c d e f]
;;                  (l/== q [[a b] [c d] [e f]])
;;                  (compatible-collo q)))
;; (l/run* [q]
;;         (l/fresh [a b c d e f]
;;                  (l/everyg (fn [x] (fd/in x (fd/domain 1 2 3)))
;;                            [a b c d e f])
;;                  (l/== q [[a b] [c d] [e f]])
;;                  (l/distincto q)
;;                  (compatible-collo q)))



(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])

(trajectories sl-3-3 (first (transducer sl-3-3 3)))

(def sl-3-3b
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]
   ["___" :none]])

(check sl-3-3b (first (transducer sl-3-3b 4)))

;; non-partial output
(def ex1
  [["aaba" :foo]
   ["bb" :bar]
   ["bababc" :foobar]
   ["bba" :foo]
   ["c" :bar]
   ["cac" :foo]
   ["ccaabb" :foobar]
   ])

(trajectories ex1 (first (transducer ex1 3)))