(ns kigen.holonomy.cayley-graph
  "Computing Cayley-graphs.")

;; elts - a set of elements
;; afs - operations that act on elts, i.e. functions: elt -> elt
(defn cayley-graph
  "Unlabelled Cayley graph of elements elts by action functions afs.
  Returns a map: element -> image set (successors) of element.
  The output can also be considered as the successor relation."
  [elts afs]
  (into {} (for [x elts]
             [x (set (for [o afs] (o x)))])))
