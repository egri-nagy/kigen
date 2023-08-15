(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[taoensso.timbre :as timbre])
(require '[tangle.core :as tangle])
(require '[clojure.java.io :refer [copy file]])

;; levels: :warn, :info, :debug
(timbre/set-min-level! :info)

(defn DotSolution

  [io-pairs {omega :omega delta :delta}]
  (let [nodes (map
               (fn [state]
                 {:id (str "node" state)
                  :label (str state " " (omega state))})
               (range (count (first (vals delta)))))
        edges (mapcat
               (fn [input-sym]
                 (map
                  (fn [a b]
                    [(str "node" a) (str "node" b) {:label input-sym}])
                  (range) (delta input-sym)))
               (input-symbols-fn io-pairs))]
    (tangle/graph->dot
     nodes
     edges
     {:directed? true ;:node {:shape :box}
      :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
      :node->descriptor (fn [n] (when-not (keyword? n) n))})))

(defn DotSolution2PDF
  [dotstring name]
  (copy (tangle/dot->image dotstring
                           "pdf")
        (file (str name ".pdf"))))


;;SIGNAL LOCATORS
;; where is the 'pulse'?
;; sl-n-k signal locator for n symbols with k regions 
;; the number of state is checked to be minimal
(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])
(def sl-3-3sol (first (f/transducer sl-3-3 3)))
(println (trajectories sl-3-3 sl-3-3sol))

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

;;(def plndrm4 (palindromes 4))
;;(first (f/transducer plndrm4 5))

;; (def plndrm5 (palindromes 5))
;; (first (transducer plndrm5 6))) ;??

;;can we recover the exact same automaton?
;; T has 4 states and 3 input symbols
(def T [[0 3 1 3] ;states transformed by input symbol 0
        [1 2 0 3]
        [1 0 2 3]])

(def Ti-o-pairs
  (for [w (repeatedly 7
                      (fn [] (vec (repeatedly 4
                                              (partial rand-int 3)))))]
    [w (process-word T 0 w)]))

;is it uniquely determined?
(count (f/transducer Ti-o-pairs 4))

;;COUNTING ONES : length of input word + 1 states needed
(first
 (f/transducer
  (map (fn [l]
         [l (count (filter #{1} l))])
       (combo/selections [0 1] 4)) 5))

;; deciding whether there are more zeroes or ones, or equal
;; not easy, for 4 inputs minimum 9 states needed - better with flexible output?
(def zo
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) :moreones
                       (= zeroes ones) :eq
                       :else :morezeros))])
        (mapcat #(combo/selections [0 1] %) [1 2 3 4])))

(def zosol (first (f/transducer zo 5)))
(trajectories zo zosol)

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
(DotSolution2PDF (DotSolution binary binarysol) "binary")

(def binary2
  [[[0 0 0] :0]
   [[1 0 0] :1]
   [[0 1 0] :2]
   [[1 1 0] :3]
   [[0 0 1] :4]
   [[1 0 1] :5]
   [[0 1 1] :6]
   [[1 1 1] :7]])
(def binary2sol  (first (ft/transducer binary2 8)))
(trajectories binary2 binary2sol)
(check binary2 binary2sol)
(DotSolution2PDF (DotSolution binary2 binary2sol) "binary2")



(spit "sl.dot" (DotSolution sl-3-3 sl-3-3sol))
(DotSolution2PDF (DotSolution sl-3-3 sl-3-3sol) "sl-3-3")

(DotSolution2PDF zo zosol "zo")
(DotSolution2PDF binary binarysol "binary")

(def nodes [:a :b :c :d {:id (str :d) :label "luki"}])
(def edges [[:a :b] [:b :c]])
(copy (tangle/dot->image (tangle/graph->dot
                          nodes
                          edges
                          {:directed? true :node {:shape :box}
                           :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
                           :node->descriptor (fn [n] (when-not (keyword? n) n))})
                         "pdf")
      (file "x.pdf"))

(defn get-maps
  " "
  [delta initial-state input-word]
  (second
   (reduce
    (fn [[state maps] input]
      [((delta input) state)
       (conj maps [state input])])
    [initial-state #{}]
    input-word)))

(defn get-all-maps
  [io-pairs {delta :delta}]
  (let [ms (map (partial get-maps delta 0) (map first io-pairs))]
    (reduce into ms)))

(count (get-all-maps binary binarysol))