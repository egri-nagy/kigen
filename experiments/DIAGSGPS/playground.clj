(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as transf])
(require '[kigen.semigroup.conjugacy :as c])

;(deftest gentab)

(def T2gens (transf/full-ts-gens 2))
(def T3gens (transf/full-ts-gens 3))

(call-embedding T2gens transf/mul T3gens transf/mul)


(count (embedding-backtrack T2gens
                            transf/mul
                            (index-period-matched T2gens transf/mul T3gens transf/mul)
                            transf/mul true))

(count (sgp-embeddings-by-gens T2gens
                               transf/mul
                               T3gens
                               transf/mul))


(def r
  (let [T1gens (transf/full-ts-gens 1)
        T4gens (transf/full-ts-gens 4)
        S3 (sgp-by-gens (transf/symmetric-gens 3) transf/mul)
        S4 (sgp-by-gens (transf/symmetric-gens 4) transf/mul)
        tmul transf/mul
        tconj transf/conjugate]
    (sgp-embeddings-by-gens T2gens tmul
                            T3gens tmul
                            (c/conjugation-fn-bundle tconj S3))))


(map (fn [gens] (sgp-by-gens (map second gens) transf/mul)) r)

(def S3 (sgp-by-gens (transf/symmetric-gens 3) transf/mul))

(defn onpt
  [pt p]
  (p pt))

(c/seqconjrep onpt [1 2 1]  S3)

(c/conj-conj onpt [[] S3] 2)

(reductions
 (fn [X pt]
   (c/conj-conj onpt X pt))
 [[] S3]
 [2 2 1 0 1 2])

;;;;; New canonical labeling algorithms ;;;;;;;;;;;;;;;;;;;;;;;
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

(defn can-seq
  [s]
  (let [[m _] (can-lab-seq s)]
    (mapv m s)))

(defn can-lab-seq-seq
  "Canonical labeling of a sequence of sequences. It simply threads partial
   labelings through the sequence by calling [[can-lab-seq]].
   It returns the labeling."
  [sseq]
  (let [[m _] (reduce can-lab-seq
                      (can-lab-seq (first sseq))
                      (rest sseq))]
    m))

(defn can-seq-seq
  "The canonical representation of a sequence of sequences."
  [sseq]
  (mapv (partial mapv (can-lab-seq-seq sseq)) sseq))

(defn can-lab-set-seq
  "Canonical labeling of a set of sequences."
  [sset]
  (let [boo (fn [[done waiting PL]]
              (let [s2PL (zipmap waiting
                                 (map (partial can-lab-seq PL) waiting))
                    canon-form (fn [s] (mapv (first (s2PL s)) s))
                    cans2s (group-by canon-form waiting)
                    minrep (first (sort compare (keys cans2s)))]
                (for [t (cans2s minrep)]
                  [(conj done minrep)
                   (remove #{t} waiting)
                   (s2PL t)])))]
    (loop [labelings [[[] sset [{} 0]]]] ;starting with the empty labeling
      (if (empty? (second (first labelings))) ;i.e., done is empty
        (map (fn [[rep _ p]] [rep p]) labelings) ;the output
        (let [nlabelings (mapcat boo labelings)
              latest2labs (group-by (comp peek first) ;only the last can differ
                                    nlabelings)
              minrep (first (sort compare (keys latest2labs)))]
          (recur (latest2labs minrep)))))))

(defn can-set-seq
  [sset]
  (first (first (can-lab-set-seq sset))))