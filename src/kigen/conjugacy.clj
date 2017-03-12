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
