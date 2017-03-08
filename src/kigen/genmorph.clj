(ns kigen.genmorph
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]))

(declare extend-morph add-edge extend-node add-gen-and-close)

;; sorting with duplicates removed, like UNIX's sort -u
(def sort-u (comp vec sort set))

(defn minconjugators
  "Finds the minimal conjugate transformation of t under permutations G.
  Also returns the subset of G that takes t to the rep."
  [t G]
  (let [conjugations (map
                      (fn [p] [(transf/conjugate t p) p])
                      G)
        mint (first (sort (map first conjugations)))]
    [mint, (map second (filter #(= mint (first %)) conjugations))]))

(defn conj-conj
  "A conjugated list is a sequence of transformation and a set of permutations.
  The list is extended by a new transformation, conjugated to make it minimal
  in the conjugacy class. Set of possible conjugators reduced accordingly."
  [[L G] t]
  (let [[mint nG] (minconjugators t G)]
    [(conj L mint) nG]))

(defn conjrep
  "The conjugacy class representative of a sequence of transformations L under
  permutations G."
  [L G]
  (first
    (reduce conj-conj [[] G] L)))

(defn conjrep?
  "True, if the given sequence of transformations is its conjugacy class rep."
  [L G]
  (= L (conjrep L G)))

(defn setconjrep
  [coll G]
  (let [ccl (pmap #(vec (sort (map (fn [x] (transf/conjugate x %))
                                  coll)))
                 G)]
    (first (sort-u ccl))))

(defn source
  "data items of source semigroup"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))
        gentab (vec (pmap
                     (fn [x] (->> gens
                                  (map #(mul x %))
                                  (map #(indices %))
                                  (vec)))
                     elts))]
    {:gens (range (count gens))
     :gentab gentab
     :elts elts
     :indices indices
     :mul (fn [x y] ((gentab x) y))
     :genips (map #(sgp/index-period % mul) gens)}))

(defn target
  "data items of target semigroup"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        classes (group-by #(sgp/index-period % mul) S)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))]
    {:gens gens
     :mul mul
     :elts elts
     :indices indices
     :classes classes}))

(defn targets [src trgt] (map #((:classes trgt) %)
                              (:genips src)))

(defn embedding-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt]
  (map (fn [l] (zipmap (:gens src) l))
       (apply cartesian-product (targets src trgt))))

(defn conj-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt G]
  (let [tgs (targets src trgt)]
    (map (fn [l] (zipmap (:gens src) l))
         (filter #(conjrep? % G)
                 (apply cartesian-product tgs)))))

(defn conj-seeds2
  [src trgt G]
  (let [tgs (targets src trgt)
        seeds (loop [pseeds  [ [[] G] ]
                     candsets tgs]
                (if (empty? candsets)
                  (map first pseeds)
                  (let [X (cartesian-product pseeds (first candsets))]
                    (recur (map (fn [[a b]] (conj-conj a b)) X)
                           (rest candsets)))))]
    (map (fn [l] (zipmap (:gens src) l)) (set seeds))))

(defn embeddings
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(extend-morph % (:gens src)  (:gens src) (:mul src) (:mul trgt))
                         (embedding-seeds src trgt))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul G]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(extend-morph % (:gens src) (:gens src) (:mul src) (:mul trgt))
                         (conj-seeds2 src trgt G))))))

(defn embeddings-conj2
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul G]
  (let [[tgs mSgens mSmul] (let [src (source Sgens Smul) ;not to keep src, trgt
                                 trgt (target Tgens Tmul)]
                             [(targets src trgt), (:gens src) (:mul src)])]
    (loop [n 0, morphconjpairs [ [ {}, [[] G] ] ]]
      (if (= n (count Sgens))
        (map first (vals (group-by ;why do we need to do this?
                          #(setconjrep (vals  %) G)
                          (distinct (map first morphconjpairs)))))
        (let [ngens (nth tgs n)
              nmcprs (mapcat
                      (fn [[phi cL]]
                        (let [ncongs (map #(conj-conj cL %) ngens)
                              f (fn [[mcprs imgs] cng]
                                  (let [nmorph (add-gen-and-close
                                                phi
                                                n
                                                (last (first cng))
                                                (take (inc n) mSgens)
                                                mSmul
                                                Tmul)]
                                    (if (and (coll? nmorph)
                                             (apply distinct? (vals nmorph)))
                                      (let [img (setconjrep (vals nmorph) G)]
                                        (if (contains? imgs img)
                                          [mcprs imgs]
                                          [(conj mcprs [nmorph cng])
                                           (conj imgs img)]))
                                      [mcprs imgs])))]
                          (first (reduce f [[] #{}] ncongs))))
                      morphconjpairs)]
          (println (count nmcprs) "on" n)
          (recur (inc n) nmcprs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching - next 3 functions are nested, top to bottom ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extend-morph
  "Extends partial morphism systematically by the generators starting at the
  frontline. If morphism is not possible, returns the number of matchings."
  [phi front Sgens Smul Tmul]
  (loop [phi phi, stack (vec front)]
    (if (empty? stack)
      phi
      (let [result (extend-node phi (peek stack) Sgens Smul Tmul)]
        (if (number? result)
          result
          (recur (:phi result) (into (pop stack) (:new result))))))))

;; all nodes 1 new generator
(defn add-gen
  " "
  [phi gen phiofgen Smul Tmul]
  (loop [phi (conj phi [gen phiofgen]), incoming [gen], front (conj  (keys phi) gen)]
    (if (empty? front)
      {:phi phi :new incoming}
      (let [p (add-edge phi (first front) gen Smul Tmul)]
        (cond (number? p) p
              (empty? p) (recur phi
                                incoming
                                (rest front))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest front)))))))

(defn add-gen-and-close
  " "
  [phi gen phiofgen Sgens Smul Tmul]
  ;;(println  "agac" phi gen phiofgen Sgens)
  (let [res (add-gen phi gen phiofgen Smul Tmul)]
    ;;(println "RES" res)
    (if (number? res)
      res
      (extend-morph (:phi res) (:new res) Sgens Smul Tmul))))



;; 1-node all generators
(defn extend-node
  "Extending a single element by all generators.
  Returns the updated morphism phi and the newly added nodes."
  [phi a gens Smul Tmul]
  (loop [phi phi, incoming [], gens gens]
    (if (empty? gens)
      {:phi phi :new incoming}
      (let [p (add-edge phi a (first gens) Smul Tmul)]
        (cond (number? p) p
              (empty? p) (recur phi
                                incoming
                                (rest gens))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest gens)))))))
;; 1-node 1-generator
(defn add-edge
  "Extends the morphism phi by applying a generator b to a single element a.
  phi - morphism represented as a map
  a - element to be extended
  b - generator of S
  It returns the number of matched elements so if it is not homomorphic,
  [] when homomorphic but no new element,
  [ab AB] where AB is the newly assigned image of the product ab."
  [phi a b mulS mulT]
  ;;(println "AE" phi a b)
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (if (= AB (phi ab))
        []
        (count phi))
      [ab AB])))

;; shadow
(defn conjclass
  [L G]
  (sort-u (map #(mapv (fn [x] (transf/conjugate x %)) L) G)))

(defn conjrep-shadow [l G] (first (conjclass l G)))
