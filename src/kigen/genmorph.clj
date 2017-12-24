(ns kigen.genmorph
  "Constructing morphisms by generators, i.e. searching for an isomorphisms of
  Cayley-graphs."
  (:require [kigen.sgp :as sgp]
            [kigen.conjugacy :as conjugacy]
            [kigen.sgp :refer [sgp-by-gens]]
            [orbit.core :refer [acyclic-search-single]]
            [clojure.core.reducers :as r]))

(declare extend-morph ;; low-level morphism checking/extending functions
         add-edge
         add-gen
         extend-node
         add-gen-and-close
         embeddings ;; high-level function for finding embeddings
         embeddings-conj
         sgp-embeddings-by-gens ;;main entry point
         index-period-matched) ;;preparation

(defn gentab
  "Right generation table for semigroup given by generator elements and
  multiplication."
  [gens mul]
  (let [S (sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S))) ; why putting the generators in the front? confirmed: someone does assume order
        indices (zipmap elts (range (count elts)))
        gt (vec (pmap
                 (fn [x] (->> gens
                              (map #(mul x %))
                              (map indices)
                              (vec)))
                 elts))]
    [(range (count gens)) ;generators
     (fn [x y] ((gt x) y))])) ;multiplication


(defn index-period-matched
  "Returns for each generator in S, the elements of T with matching index-period
  values. WARNING: It fully enumerates T."
  [Sgens Smul Tgens Tmul]
  (let [ipfunc (fn [mul] (fn [x] (sgp/index-period x mul)))
        S-index-period (ipfunc Smul)
        T-index-period (ipfunc Tmul)
        T (sgp/sgp-by-gens Tgens Tmul)
        Tip->Tset (group-by T-index-period T)]
    (map Tip->Tset (map  S-index-period Sgens))))

(defn sgp-embeddings-by-gens
  "Computes all embeddings from source semigroup to target semigroup.
  Semigroups are given by generators and their multiplication functions. Source
  semigroup is replaced by its generation table. It returns a list of maps
  containing the images of the source generators, or an empty list.
  Results are up to conjugation if conjugation action and symmetries are given."
  ([Sgens Smul Tgens Tmul] ; ALL EMBEDDINGS
   (let [[mSgens mSmul] (gentab Sgens Smul)
         tgs (index-period-matched mSgens mSmul Tgens Tmul)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings mSgens mSmul tgs Tmul))))
  ([Sgens Smul Tgens Tmul Tconj G] ; ALL DISTINCT EMBEDDINGS UP TO CONJUGATION
   (let [[mSgens mSmul] (gentab Sgens Smul)
         ts (index-period-matched mSgens mSmul Tgens Tmul)
         conjrep #(conjugacy/conjrep Tconj % G)
         tgs (cons (distinct (pmap conjrep (first ts))) (rest ts))
         setconjrep #(conjugacy/setconjrep Tconj % G)
         conj-conj (conjugacy/conj-conj-fn Tconj G)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings-conj mSgens mSmul tgs Tmul
                           conjrep conj-conj setconjrep)))))

(defn embeddings
  "All embeddings of source semigroup into target induced by the possible
  images of the generators."
  [Sgens Smul tgs Tmul]
  (let [solution? (fn [[n m]] (= n (count Sgens)))
        generator (fn [[n m :as v]]
                    (if (solution? v)
                      []
                      (let [nmorphs (map
                                     (fn [g]
                                       (add-gen-and-close
                                        m
                                        (nth Sgens n)
                                        g                                                            
                                        (take (inc n) Sgens)
                                        Smul
                                        Tmul))
                                     (nth tgs n))
                            filtered (filter #(apply distinct? (vals %))
                                             (remove nil? nmorphs))]
                        (map (fn [x] [(inc n) x]) filtered))))]
    (map second (acyclic-search-single [[0 {}]]
                           generator
                           solution?))))

(defn morph-distinguisher
  "Returns the distinct morphs up to conjugation in an efficient way by
  calculating the frequencies of conjugacy class representatives. S4 -> S7
  shows that this is just a heuristics."
  [morphs repconj setconjrep]
  (let [selected (map first
                      (vals (group-by #(set (vec (vals %)))morphs)))
        conjrepfreqs (fn [coll] (frequencies (map repconj coll)))
        classes (group-by #(conjrepfreqs (vals %)) selected)
        easykeys (filter #(= 1 (count (classes %)))  (keys classes))
        hardkeys (filter #(< 1 (count (classes %)))  (keys classes))
        hard (mapcat classes hardkeys)]
    (println (count morphs) "->" (count selected))
    (println (map count (vals classes)))
    (concat (mapcat classes easykeys)
            (map first (vals (group-by #(setconjrep (vec (vals %))) hard))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul tgs Tmul repconj conj-conj setconjrep]
  (println (count (first tgs)) " candidate(s) for 1st generator")
  (loop [n 0, morphs [{}] ]
    (if (= n (count Sgens))
      ;(map first (vals (group-by #(setconjrep (vec (vals %))) morphs)))
      (morph-distinguisher morphs repconj setconjrep)
      (letfn [(extend-phi [phi]
                (let [ngens (if (zero? n)
                              (map repconj (first tgs))
                              (let [gens (mapv phi (take n Sgens))
                                    partconj (reduce conj-conj
                                                     (conj-conj (first gens))
                                                     (rest gens))]
                                (into #{}
                                      (r/map (comp last first)
                                             (r/map #(conj-conj partconj %)
                                                    (nth tgs n))))))
                      check-gen (fn [imgs2morphs ngen]
                                  (let [gens (take (inc n) Sgens)
                                        nmorph (add-gen-and-close
                                                phi
                                                (nth Sgens n)
                                                ngen
                                                gens
                                                Smul
                                                Tmul)]
                                    (if (and (coll? nmorph)
                                             (apply distinct? (vals nmorph)))
                                      (let [img (setconjrep
                                        ;we need to keep distinct generator sets
                                                 (vec
                                                  (sort
                                                   (map nmorph gens))))]
                                        (if (contains? imgs2morphs img)
                                          imgs2morphs
                                          (assoc imgs2morphs img nmorph)))
                                      imgs2morphs)))]
                  (reduce check-gen {} ngens)))]
        (let [nmorphs (vals (apply merge (pmap extend-phi morphs)))]
          (println "gens:" (inc n) "morphs:" (count nmorphs))
          (recur (inc n) nmorphs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching - next 3 functions are nested, top to bottom ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-gen-and-close
  "Add a new generator and close the Cayley-graph."
  [phi gen phiofgen Sgens Smul Tmul]
  (let [res (add-gen phi gen phiofgen Smul Tmul)]
    (if (nil? res)
      res
      (extend-morph (:phi res) (:new res) Sgens Smul Tmul))))

(defn add-gen
  "Extends partial morphism phi by adding one  new generator, i.e. multiplying
  all existing nodes. The morphic image of the generator is also needed."
  [phi gen phiofgen Smul Tmul]
  (loop [phi (conj phi [gen phiofgen])
         incoming [gen]
         front (conj  (keys phi) gen)]
    (if (empty? front)
      {:phi phi :new incoming}
      (let [p (add-edge phi (first front) gen Smul Tmul)]
        (cond (nil? p) p
              (empty? p) (recur phi
                                incoming
                                (rest front))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest front)))))))

(defn extend-morph
  "Extends partial morphism systematically by the generators starting at the
  frontline. If morphism is not possible, returns the number of matchings."
  [phi front Sgens Smul Tmul]
  (loop [phi phi, stack (vec front)]
    (if (empty? stack)
      phi
      (let [result (extend-node phi (peek stack) Sgens Smul Tmul)]
        (if (nil? result)
          result
          (recur (:phi result) (into (pop stack) (:new result))))))))


(defn extend-node
  "Extending a single element by all generators one-by-one, so breach of
  morphism gets detected immediately.
  Returns the updated morphism phi and the newly added nodes."
  [phi a gens Smul Tmul]
  (loop [phi phi, incoming [], gens gens]
    (if (empty? gens)
      {:phi phi :new incoming}
      (let [p (add-edge phi a (first gens) Smul Tmul)]
        (cond (nil? p) p
              (empty? p) (recur phi incoming (rest gens))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest gens)))))))

(defn add-edge
  "Extends the morphism phi by applying a generator b to a single element a.
  This is where the homomorphism condition is checked.
  phi - morphism represented as a map
  a - an elementof S to be extended, already in phi (TODO better logic here)
  b - a generator of S, already in phi
  It returns the number of matched elements so far if it is not homomorphic,
  [] when homomorphic but no new element generated,
  [ab AB] where AB is the newly assigned image of the product ab."
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (if (= AB (phi ab))
        []
        nil)
      [ab AB])))
