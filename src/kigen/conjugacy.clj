(ns kigen.conjugacy
  "Abstract function for calculating conjugate elements, conjugacy classes,
  representatives.")

(defn conjrep-general
  [conjugation-function thing symmetries]
  (letfn [(f [minimal-thing sym]
            (let [new-thing (conjugation-function thing sym)]
              (if (< (compare new-thing minimal-thing) 0)
                new-thing
                minimal-thing)))]
    (reduce f thing symmetries)))

(defn minconjugators-general
  "Finds the minimal conjugate transformation of t under permutations G.
  Also returns the subset of G that takes t to the rep."
  [conjugation-function t G]
  (let [conjugations (map
                      (fn [p] [(conjugation-function t p) p])
                      G)
        mint (first (sort (map first conjugations)))]
    [mint, (map second (filter #(= mint (first %)) conjugations))]))
