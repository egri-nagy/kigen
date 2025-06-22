(ns kigen.semigroup.conjugacy
  "Abstract functions for calculating conjugate elements, conjugacy classes,
  and representatives.")

;;; conjugating a single thing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn conjrep-by-minimum
  "Naive brute force implementation of finding the minimal conjugacy class
  representative of a thing by going through all of its conjugates
  induced by the given symmetries using a conjugation function.
  The conjugation function has the form: thing symmetry -> conjugated thing.
  Assumption is that things can be compared."
  [conjugation-function thing symmetries]
  (reduce (fn [minimal-thing sym]
            (let [new-thing (conjugation-function thing sym)]
              (if (neg? (compare new-thing minimal-thing))
                new-thing
                minimal-thing)))
          thing
          symmetries))

(defn min-rep-and-class
  "Finds the minimal conjugacy class representative and its class
  using the given function for calculating representatives in the
  collection T. Returns a pair of the minimal representative and
  its conjugates from T (may not be the whole class).
  Same as conjrep-by-minimum, but collects conjugate things."
  [T conjrepfunc]
  (reduce
   (fn [[m mc :as db] t]
     (let [r (conjrepfunc t)
           flag (compare r m)]
       (cond (neg? flag) [r [t]]
             (zero? flag) [m (conj mc t)]
             :else db)))
   [(conjrepfunc (first T)) [(first T)]]
   (rest T)))

(defn minconjugators
  "Finds the minimal conjugate of t under permutations G.
  Also returns the subset of G that takes t to that rep."
  [conjugation-function t symmetries]
  (reduce (fn [[mint conjugators :as r] p]
            (let [nt (conjugation-function t p)
                  comparison-result (compare nt mint)]
              (cond  (neg? comparison-result) [nt [p]]
                     (zero? comparison-result) [mint (conj conjugators p)]
                     :else r)))
          [(conjugation-function t (first symmetries)) ;first conjugate
           [(first symmetries)]] ;and its conjugators
          (rest symmetries)))

;;; conjugating a set of things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn conjugateset
  "Generalized conjugation function for sets."
  [conjugation-function things sym]
  (vec ;why vector? because it is Comparable
   (apply sorted-set ; sorted-set to be on the safe side with duplicates
    (map (fn [x] (conjugation-function x sym))
         things))))

(defn setconjrep
  "Given a set of things, it returns the conjugacy class representative set
  (the minimal possible in lexicographic order)."
  [conjugation-function things symmetries]
  (conjrep-by-minimum (partial conjugateset conjugation-function)
           (vec (apply sorted-set things))
           symmetries))

;;; conjugating a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the idea is to extend the sequence with a decreasing number of symmetries
; later in the sequence only the conjugators for the first minimal one can be
; used, so they only get smaller, once down to the identity we can just add
; the next
(defn conj-conj
  "Conjoining the next minimal conjugate to list L."
  [conjugation-function [L G] t]
  (let [[mint nG] (minconjugators conjugation-function t G)]
    [(conj L mint) nG]))

(defn conj-conj-fn
  "Conjoing a new element to a conjugate rep sequence, or starting a new
  sequence."
  [conjugation-function G]
  (fn
    ([t] (let [[r nG] (minconjugators conjugation-function t G)]
           [[r] nG]))
    ([[L G] t]
     (let [[r nG] (minconjugators conjugation-function t G)]
       [(conj  L r) nG]))))

(defn seqconjrep
  "The conjugacy class representative of a sequence of elements L under
  permutations G."
  [conjugation-function L G] ;naming problem? this L is not the same as above?
  (first (reduce (partial conj-conj conjugation-function) [[] G] L)))

(defrecord ConjugationFunctionBundle [conjrep setconjrep conjconj])

(defn conjugation-fn-bundle [conjugation-function G]
  (->ConjugationFunctionBundle #(conjrep-by-minimum conjugation-function % G)
                               #(setconjrep conjugation-function % G)
                               (conj-conj-fn conjugation-function G)))
