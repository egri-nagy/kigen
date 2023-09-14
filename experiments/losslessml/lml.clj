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
  (if-let [transducer (first  (transducer-function io-pairs n))]
    ;then
    (let [partial (partial-transducer io-pairs transducer)]
      (doseq [l (trajectories io-pairs transducer)]
        (println l))
      (println "Check partial:" (check io-pairs partial))
      (println (degrees-of-freedom partial))
      (println partial))
    ;else
    "no solution"))

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

;too long
;(def plndrm5 (palindromes 5))
;(experiment "palindromes 5 flexible" plndrm5 32 f/transducer)

;;can we recover the exact same automaton?
;; T has 4 states and 3 input symbols
(def T [[0 3 1 3] ;states transformed by input symbol 0
        [1 2 0 3]
        [1 0 2 3]])

(def Ti-o-pairs
  (for [w (repeatedly 3
                      (fn [] (vec (repeatedly 4
                                              (partial rand-int 3)))))]
    [w (result-state T 0 w)]))

;is it uniquely determined? we can measure how partial it is
(experiment "starting from T flexible" Ti-o-pairs 4 f/transducer)

;;COUNTING ONES : length of input word + 1 states needed
(first
 (f/transducer
  (map (fn [l]
         [l (count (filter #{1} l))])
       (combo/selections [0 1] 4)) 5))

;; deciding whether there are more zeroes or ones, or equal
(defn zof
  [n]
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) :moreones
                       (= zeroes ones) :eq
                       :else :morezeros))])
        (mapcat #(combo/selections [0 1] %) (range 1 (inc n)))))

(def zo3 (zof 3))
(experiment "more zeroes or more ones flexible - max 3"
            zo3 5 f/transducer)

;interestingly this is the same
(def zo4 (zof 4))
(experiment "more zeroes or more ones flexible - max 4"
            zo4 5 f/transducer)

;??
;(def zo5 (zof 5))
;(experiment "more zeroes or more ones flexible - max 5"
;            zo5 6 f/transducer)


(def parity
  [[[0 0] :0]
   [[0 1] :1]
   [[1 0] :1] 
   [[1 1] :0]])
(experiment "parity flexible" parity 2 f/transducer)
(experiment "parity from trajectories" parity 2 ft/transducer)

(def binary
  [[[0 0 0] :0]
   [[0 0 1] :1]
   [[0 1 0] :2]
   [[0 1 1] :3]
   [[1 0 0] :4]
   [[1 0 1] :5]
   [[1 1 0] :6]
   [[1 1 1] :7]])
(experiment "binary encoding flexible" binary 8 f/transducer)
(experiment "binary encoding from trajectories" binary 8 ft/transducer)

(def reversed-binary
  (mapv
   (fn [[ word output]]
     [(reverse word) output])
   binary))

(experiment "reversed binary encoding flexible"
            reversed-binary 8 f/transducer)
(experiment "reversed binary encoding from trajectories"
            reversed-binary 8 ft/transducer)