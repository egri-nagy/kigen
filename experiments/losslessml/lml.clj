(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transducer.viz :refer [DotTransducer Dot2PDF]])

;; levels: :warn, :info, :debug
(timbre/set-min-level! :info)

;;SIGNAL LOCATORS
;; where is the 'pulse'?
;; sl-n-k signal locator for n symbols with k regions 
;; the number of state is checked to be minimal
(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])
(def sl-3-3sol (first (ft/transducer sl-3-3 3)))
(println (trajectories sl-3-3 sl-3-3sol))

(def sl-3-3b
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]
   ["___" :none]])
(def sl-3-3bsol (first (ft/transducer sl-3-3b 4)))

(def sl-6-2
  [["|_____" :first]
   ["_|____" :first]
   ["__|___" :first]
   ["___|__" :second]
   ["____|_" :second]
   ["_____|" :second]])
(first (f/transducer sl-6-2 4))

(def sl-6-3
  [[[1 0 0  0 0 0] :beginning]
   [[0 1 0 0 0 0] :beginning]
   [[0 0 1 0 0 0] :middle]
   [[0 0 0 1 0 0] :middle]
   [[0 0 0 0 1 0] :end]
   [[0 0 0 0 0 1] :end]])
(first (f/transducer sl-6-3 4))

(def sl-6-6
  [[[1 0 0  0 0 0] :1]
   [[0 1 0 0 0 0] :2]
   [[0 0 1 0 0 0] :3]
   [[0 0 0 1 0 0] :4]
   [[0 0 0 0 1 0] :5]
   [[0 0 0 0 0 1] :6]])
(first (f/transducer sl-6-6 6))

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
(first (f/transducer plndrm3 4))

(def plndrm4 (palindromes 4))
(def plndrm4sol (first (f/transducer plndrm4 5)))
(Dot2PDF (DotTransducer plndrm4 plndrm4sol) "palindrome")

; (def plndrm5 (palindromes 5))
; (first (f/transducer plndrm5 7)) ;??

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



(spit "sl.dot" (DotTransducer sl-3-3 sl-3-3sol))
(Dot2PDF (DotTransducer sl-3-3 sl-3-3sol) "sl-3-3")

(spit "slb.dot" (DotTransducer sl-3-3b sl-3-3bsol))
(Dot2PDF (DotTransducer sl-3-3b sl-3-3bsol) "sl-3-3b")
(trajectories sl-3-3b sl-3-3bsol)

(defn extract-maps
  "Collects all the maps from applying the transducer to an input word. It computes the trajectory, and labels the state transitions with input symbols."
  [delta initial-state input-word]
  (let [traj (trajectory delta initial-state input-word)]
    (map vector input-word
         (map vec (partition 2 1 traj)))))

(defn extract-all-maps
  [io-pairs {delta :delta}]
  (let [maps (map (partial extract-maps delta 0)
                  (map first io-pairs))
        by-input (group-by first (reduce into #{} maps))]
    (update-vals
     by-input
     (fn [ms]
       (into {} (map second ms))))))

;; (defn partial-state-transition-table
;;   [extracted-maps n]
;;   )

(count (extract-all-maps binary binarysol))