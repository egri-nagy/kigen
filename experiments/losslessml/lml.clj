(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transducer.viz :refer [DotTransducer Dot2PDF]])

;; levels: :warn, :info, :debug
(timbre/set-min-level! :info)

(defn degrees-of-freedom
  [{delta :delta omega :omega}]
  (let [inputs (count delta)
        states (count omega)
        dof (* states (inc inputs))
        nils (count (filter nil? (apply concat omega (vals delta))))]
    [(- dof nils) dof]))

(defn experiment
  [name io-pairs n transducer-function]
  (println name)
  (let [transducer (first  (transducer-function io-pairs n))
        partial (partial-transducer io-pairs transducer)]
   (doseq [l (trajectories io-pairs transducer)]
     (println l))
    (println "Check partial:" (check io-pairs partial))
    (println (degrees-of-freedom partial))))

;;SIGNAL LOCATORS
;; where is the 'pulse'?
;; sl-n-k signal locator for n symbols with k regions 
;; the number of state is checked to be minimal
(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])
(experiment "sl-3-3 flexible" sl-3-3 3 f/transducer)
(experiment "sl-3-3 from trajectories"  sl-3-3 3 ft/transducer)
(experiment "sl-3-3 from trajectories" sl-3-3 4 ft/transducer)

(def sl-3-3b
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]
   ["___" :none]])
(experiment "sl-3-3b flexible" sl-3-3b 4 f/transducer)
(experiment "sl-3-3b from trajectories" sl-3-3b 4 ft/transducer)


(def sl-6-2
  [["|_____" :first]
   ["_|____" :first]
   ["__|___" :first]
   ["___|__" :second]
   ["____|_" :second]
   ["_____|" :second]])
(experiment "sl-6-2 flexible" sl-6-2 4 f/transducer)
; takes too long (experiment "sl-6-2 from trajectories" sl-6-2 4 ft/transducer)

(def sl-6-3
  [[[1 0 0  0 0 0] :beginning]
   [[0 1 0 0 0 0] :beginning]
   [[0 0 1 0 0 0] :middle]
   [[0 0 0 1 0 0] :middle]
   [[0 0 0 0 1 0] :end]
   [[0 0 0 0 0 1] :end]])
(experiment "sl-6-3 flexible" sl-6-3 4 f/transducer)

(def sl-6-6
  [[[1 0 0  0 0 0] :1]
   [[0 1 0 0 0 0] :2]
   [[0 0 1 0 0 0] :3]
   [[0 0 0 1 0 0] :4]
   [[0 0 0 0 1 0] :5]
   [[0 0 0 0 0 1] :6]])
(experiment "sl-6-3 flexible" sl-6-3 6 f/transducer)

(def sl-9-3
  [[[1 0 0  0 0 0  0 0 0] :1st]
   [[0 1 0 0 0 0 0 0 0] :1st]
   [[0 0 1 0 0 0 0 0 0] :1st]
   [[0 0 0 1 0 0 0 0 0] :2nd]
   [[0 0 0 0 1 0 0 0 0] :2nd]
   [[0 0 0 0 0 1 0 0 0] :2nd]
   [[0 0 0 0 0 0 1 0 0] :3rd]
   [[0 0 0 0 0 0 0 1 0] :3rd]
   [[0 0 0 0 0 0 0 0 1] :3rd]])
(first (f/transducer sl-9-3 5))
(experiment "sl-9-3 flexible" sl-9-3 5 f/transducer)

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
(experiment "palindromes 3 flexible" plndrm3 4 f/transducer)

(def plndrm4 (palindromes 4))
(experiment "palindromes 4 flexible" plndrm4 5 f/transducer)

(def plndrm5 (palindromes 5))
(experiment "palindromes 5 flexible" plndrm5 32 f/transducer)

;;can we recover the exact same automaton?
;; T has 4 states and 3 input symbols
(def T [[0 3 1 3] ;states transformed by input symbol 0
        [1 2 0 3]
        [1 0 2 3]])

(def Ti-o-pairs
  (for [w (repeatedly 7
                      (fn [] (vec (repeatedly 4
                                              (partial rand-int 3)))))]
    [w (result-state T 0 w)]))

;is it uniquely determined?
(first (f/transducer Ti-o-pairs 4))

;;COUNTING ONES : length of input word + 1 states needed
(first
 (f/transducer
  (map (fn [l]
         [l (count (filter #{1} l))])
       (combo/selections [0 1] 4)) 5))

;; deciding whether there are more zeroes or ones, or equal
;; not easy, for 4 inputs minimum 9 states needed - better with flexible output?
;; (def zo
;;   (mapv (fn [l]
;;           [(vec l) (let [ones (count (filter #{1} l))
;;                          zeroes (count (filter #{0} l))]
;;                      (cond
;;                        (< zeroes ones) :moreones
;;                        (= zeroes ones) :eq
;;                        :else :morezeros))])
;;         (mapcat #(combo/selections [0 1] %) [1 2 3 4])))

;; (def zosol (first (f/transducer zo 9)))
;; (trajectories zo zosol)

;; ;;old method - seven states
;; (def zo2
;;   (mapv (fn [l]
;;           [(vec l) (let [ones (count (filter #{1} l))
;;                          zeroes (count (filter #{0} l))]
;;                      (cond
;;                        (< zeroes ones) 1
;;                        (= zeroes ones) 2
;;                        :else 3))])
;;         (combo/selections [0 1] 4)))

;; (def zo2sol (first (f/transducer zo2 7)))
;; (check zo2 zo2sol)
;; (trajectories zo2 zo2sol)

(def parity
  [[[0 0] :0]
   [[0 1] :1]
   [[1 0] :1] 
   [[1 1] :0]])
(def paritysol  (first (f/transducer parity 2)))
(trajectories parity paritysol)
(check parity paritysol)
(Dot2PDF (DotTransducer parity paritysol) "parity")

(def binary
  [[[0 0 0] :0]
   [[0 0 1] :1]
   [[0 1 0] :2]
   [[0 1 1] :3]
   [[1 0 0] :4]
   [[1 0 1] :5]
   [[1 1 0] :6]
   [[1 1 1] :7]])
(def binarysol  (first (f/transducer binary 8)))
(trajectories binary binarysol)
(check binary binarysol)
(Dot2PDF (DotTransducer binary binarysol) "binary")

(def binary2
  [[[0 0 0] :0]
   [[1 0 0] :1]
   [[0 1 0] :2]
   [[1 1 0] :3]
   [[0 0 1] :4]
   [[1 0 1] :5]
   [[0 1 1] :6]
   [[1 1 1] :7]])
(def binary2sol  (first (f/transducer binary2 8)))
(trajectories binary2 binary2sol)
(check binary2 binary2sol)
(Dot2PDF (DotTransducer binary2 binary2sol) "binary2")

;(Dot2PDF (DotTransducer sl-3-3 sl-3-3sol) "sl-3-3")