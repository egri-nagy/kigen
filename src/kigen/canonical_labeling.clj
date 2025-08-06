(ns kigen.canonical-labeling
  "For direct computations of canonical labelings of sequences. A transformation
   is a sequence, for example.
   Functions can be continued from partial labelings.")

;; Canonical labeling of a sequence
(defn can-lab-seq
  "Canonical labeling of a sequence `s`. The elements of the sequence get
   converted to non-negative integers in ascending order. A hash-map is
   constructed to map elements to integers bijectively.
   `PL`, a data structure for representing a partial labeling can be given in
   the form [mappings next-available-integer]. It can be used for canonical
   labeling of several sequences. This is also the ouput."
  ([s] (can-lab-seq [{} 0] s))
  ([PL s] ; canonical labeling data can be given
   (reduce
    (fn [[mappings available] element]
      (if (contains? mappings element)
        [mappings available]
        [(conj mappings [element available]) (inc available)]))
    PL
    s)))

;; Canonical form of a sequence
(defn can-seq
  "Returns the canonical form of a sequence `s`. It can be continued from a
   partial labeling `PL`."
  ([s] (can-seq [{} 0] s))
  ([PL s]
   (let [[m _] (can-lab-seq PL s)]
     (mapv m s))))

;; Canonical labeling for a sequence of sequences
(defn can-lab-seq-seq
  "Canonical labeling of a sequence of sequences. It simply threads partial
   labelings through the sequence by calling [[can-lab-seq]].
   It returns the labeling."
  ([sseq] (can-lab-seq-seq [{} 0] sseq))
  ([PL sseq]
   (reduce can-lab-seq
           PL
           sseq)))

;; Canonical form of a sequence of sequences
(defn can-seq-seq
  "The canonical representation of a sequence of sequences."
  ([sseq] (can-seq-seq [{} 0] sseq))
  ([PL sseq]
   (mapv (partial mapv (first (can-lab-seq-seq PL sseq))) ;extract the hash-map
         sseq)))

;; Canonical labeling of a set of sequences
(defn can-lab-set-seq
  "Canonical labeling of a set of sequences."
  [sset]
  (let [boo (fn [[waiting PL done]]
              (let [s2PL (zipmap waiting
                                 (map (partial can-lab-seq PL) waiting))
                    canon-form (fn [s] (mapv (first (s2PL s)) s))
                    cans2s (group-by canon-form waiting)
                    minrep (first (sort compare (keys cans2s)))]
                (for [t (cans2s minrep)]
                  [(remove #{t} waiting)
                   (s2PL t)
                   (conj done minrep)])))]
    (loop [labelings [[sset [{} 0] []]]] ;starting with the empty labeling
      (if (empty? (first (first labelings))) ;i.e., waiting is empty
        (map rest labelings) ;the output
        (let [nlabelings (mapcat boo labelings)
              latest2labs (group-by (comp peek peek) ;only the last can differ
                                    nlabelings)
              minrep (first (sort compare (keys latest2labs)))]
          (recur (latest2labs minrep)))))))

;; Canonical form of a set of sequences
(defn can-set-seq
  [sset]
  (second (first (can-lab-set-seq sset))))