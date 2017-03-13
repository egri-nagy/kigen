(ns kigen.conjugacy
  "Abstract function for calculating conjugate elements, conjugacy classes,
  representatives.")

(defn conjrep
  [conjugation-function thing symmetries]
  (letfn [(f [minimal-thing sym]
            (let [new-thing (conjugation-function thing sym)]
              (if (< (compare new-thing minimal-thing) 0)
                new-thing
                minimal-thing)))]
    (reduce f thing symmetries)))

(defn conjugateset
  [conjugation-function things sym]
  (vec
   (sort
    (map (fn [x] (conjugation-function x sym))
         things))))

(defn setconjrep
  [conjugation-function things symmetries]
  (conjrep (partial conjugateset conjugation-function)
           things
           symmetries))

(defn minconjugators
  "Finds the minimal conjugate transformation of t under permutations G.
  Also returns the subset of G that takes t to the rep."
  [conjugation-function t G]
  (let [conjugations (map
                      (fn [p] [(conjugation-function t p) p])
                      G)
        mint (first (sort (map first conjugations)))]
    [mint, (map second (filter #(= mint (first %)) conjugations))]))

(defn conj-conj
  "Conjoing a new element to a conjugate rep sequence."
  [conjugation-function [L G] t]
  (let [[mint nG] (minconjugators conjugation-function t G)]
    [(conj L mint) nG]))

(defn seqconjrep
  "The conjugacy class representative of a sequence of elements L under
  permutations G."
  [conjugation-function L G]
  (first (reduce (partial conj-conj conjugation-function) [[] G] L)))
