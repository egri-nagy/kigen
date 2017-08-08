(ns kigen.conjugacy
  "Abstract functions for calculating conjugate elements, conjugacy classes,
  and representatives.")

(defn conjrep
  "Naive brute force implementation of finding the minimal conjugacy class
  as the representative of a thing by going through all its conjugates
  induced by the given symmetries using a conjugation function.
  The conjugation function has the form: [thing symmetry] -> conjugated thing.
  Assumption is that things can be compared."
  [conjugation-function thing symmetries]
  (letfn [(f [minimal-thing sym]
            (let [new-thing (conjugation-function thing sym)]
              (if (neg? (compare new-thing minimal-thing))
                new-thing
                minimal-thing)))]
    (reduce f thing symmetries)))

(defn conjugateset
  "Generalized conjugation function for sets."
  [conjugation-function things sym]
  (vec
   (sort
    (map (fn [x] (conjugation-function x sym))
         things))))

(defn setconjrep
  "Given a set of things, it returns the conjugacy class representative set
  (the minimal possible in lexicographic order)."
  [conjugation-function things symmetries]
  (conjrep (partial conjugateset conjugation-function)
           (vec (sort things))
           symmetries))

(defn minconjugators
  "Finds the minimal conjugate transformation of t under permutations G.
  Also returns the subset of G that takes t to the rep."
  [conjugation-function t G]
  (reduce (fn [r p]
            (let [nt (conjugation-function t p)
                  mint (first r)
                  comparison-result (compare nt mint)]
              (cond  (neg? comparison-result) [nt [p]]
                     (zero? comparison-result) [mint (conj (second r) p)]
                     :else r)))
          [(conjugation-function t (first G)) [(first G)]]
          (rest G)))

(defn conj-conj

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
    ([ [L G] t]
     (let [[r nG] (minconjugators conjugation-function t G)]
       [(conj  L r) nG]))))

(defn seqconjrep
  "The conjugacy class representative of a sequence of elements L under
  permutations G."
  [conjugation-function L G]
  (first (reduce (partial conj-conj conjugation-function) [[] G] L)))
