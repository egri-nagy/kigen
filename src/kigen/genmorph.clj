(ns kigen.genmorph
  "Constructing morphisms by generators, i.e. searching for an isomorphisms of
  Cayley-graphs."
  (:require [kigen.sgp :as sgp]
            [kigen.conjugacy :as conjugacy]
            [kigen.sgp :refer [sgp-by-gens]]
            [orbit.core :refer [acyclic-search-single]]
            [clojure.core.reducers :as r]))

(declare extend-morph ;; low-level morphism checking/extending functions
         new-mapping
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
    ;(println (count morphs) "->" (count selected))
    ;(println (map count (vals classes)))
    (concat (mapcat classes easykeys)
            (map first (vals (group-by #(setconjrep (vec (vals %))) hard))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul tgs Tmul repconj conj-conj setconjrep]
  ;(println (count (first tgs)) " candidate(s) for 1st generator")
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
          ;(println "gens:" (inc n) "morphs:" (count nmorphs))
          (recur (inc n) nmorphs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; there are two phases for adding a new generator
;; 1. we multiply everything (including itself by the new generator)
;; 2. for any new elements generated we go through with all generators so far

(defn add-gen-and-close
  "Add a new generator and close the Cayley-graph."
  [phi gen phiofgen Sgens Smul Tmul]
  (let [res (add-gen (conj phi [gen phiofgen]) gen Smul Tmul)]
    (when-not (nil? res)
      (extend-morph (:phi res) (conj (:new res) gen) Sgens Smul Tmul))))

(defn extend-morph
  "Extends partial morphism systematically by the generators starting at the
  frontline. If morphism is not possible, returns the number of matchings."
  [phi front Sgens Smul Tmul]
  (loop [phi phi, stack (vec front)]
    (if (empty? stack)
      phi
      (let [result (extend-node phi (peek stack) Sgens Smul Tmul)]
        (when-not (nil? result)
          (recur (:phi result) (into (pop stack) (:new result))))))))

;; add-gen and extend-node are the same, but they recur on different arguments
;; it's unclear how to abstract this

(defn abstract
  "Systematic right multiplication  elts by gens and collecting new elements.
  Elts and gens are all in phi already. Phi is being built along the way
  but new elements need proper extension, that's why they are collected."
  [phi elts gens Smul Tmul]
  (loop [phi phi
         newelts []
         pairs (for [a elts b gens] [a b])]
    (if (empty? pairs)
      {:phi phi :new newelts}
      (let [v (first pairs)
            p (new-mapping phi (first v) (second v) Smul Tmul)]
        (cond (nil? p) p
              (empty? p) (recur phi
                                newelts
                                (rest pairs))
              :else (recur (conj phi p)
                           (conj elts (first p))
                           (rest pairs)))))))

(defn add-gen
  "Extends partial morphism phi by adding one  new generator, i.e. multiplying
  all existing nodes. The morphic image of the generator is also needed."
  [phi gen Smul Tmul]
    (abstract phi
              (keys phi)
              [gen]
              Smul Tmul))

(defn extend-node
  "Extends partial morphism phi by adding one  new generator, i.e. multiplying
  all existing nodes. The morphic image of the generator is also needed."
  [phi a gens Smul Tmul]
    (abstract phi
              [a]
              gens
              Smul Tmul))

(defn new-mapping
  "Extends the morphism phi by multiplying a by b and finding phi(a,b) if
  the map is homomorphic, if not it returns nil.
  If phi(a,b) is already known, then it returns an empty vector. If it is newly
  found, it gives a vector [ab phi(a,b)], that can be conjoined to phi.
  phi - morphism represented as a map
  a,b - elements of S  already in phi
  mulS, mulT - multiplication in S and T"
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (when (= AB (phi ab)) [])
      [ab AB])))
