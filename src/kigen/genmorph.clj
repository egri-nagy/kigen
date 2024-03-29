(ns kigen.genmorph
  "Constructing isomorphisms/embeddings between semigroups given  by generators.
  In other words, searching for an isomorphisms of Cayley-graphs.
  The elements of both semigroups are fully enumerated.
  The source semigroup is converted to generator table (partial multiplication
  table containing all the images of right multiplication by generators).
  The elements of target semigroups are classified by their index-periods in
  order to find possible targets for generators."
  (:require [kigen.sgp :refer [sgp-by-gens index-period ->Sgp]]
            [orbit.core :refer [ptree-search]] ;tree-search for single-threaded execution
            [clojure.core.reducers :as r]
            [kigen.memory-info :refer [mem-info]]
            [taoensso.timbre :refer [trace]]))

(declare sys-mul;; low-level morphism checking/extending functions
         new-mapping
         add-gen-and-close
         embeddings ;; high-level function for finding embeddings
         embeddings-distinct
         sgp-embeddings-by-gens ;;main entry point
         index-period-matched
         gentab) ;;preparation

(defn gentab
  "Right generation table for semigroup given by generator elements and
  multiplication. Returns the generators and the multiplication by generators
  function. When the elements are enumerated the generators are put in front.
  Otherwise, table indexing would be more complicated."
  [gens mul]
  (let [S (sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))
        gt (vec (pmap
                 (fn [x] (->> gens
                              (map #(mul x %))
                              (map indices)
                              (vec)))
                 elts))]
    (->Sgp (range (count gens)) ;generators
           (fn [x y] ((gt x) y))))) ;multiplication

(defn index-period-matched
  "Returns for each generator in S, the elements of T with matching index-period
  values. WARNING: It fully enumerates T."
  [{Sgens :gens Smul :mul} Tgens Tmul]
  (let [ipfunc (fn [mul] (fn [x] (index-period x mul)))
        S-index-period (ipfunc Smul)
        T-index-period (ipfunc Tmul)
        T (sgp-by-gens Tgens Tmul)
        Tip->Tset (group-by T-index-period T)]
    (map Tip->Tset (map  S-index-period Sgens))))

(defn sgp-embeddings-by-gens
  "Computes all embeddings from source semigroup to target semigroup.
  Semigroups are given by generators and their multiplication functions. Source
  semigroup is replaced by its generation table. It returns a list of maps
  containing the images of the source generators, or an empty list.
  Results are up to conjugation if conjugation action and symmetries are given."
  ([Sgens Smul Tgens Tmul] ; ALL EMBEDDINGS
   (let [{mSgens :gens mSmul :mul :as mS} (gentab Sgens Smul)]
     (map (fn [m] (zipmap Sgens (map m mSgens))) ; mappings of the generators
          (embeddings mSgens
                      mSmul
                      (index-period-matched mS Tgens Tmul)
                      Tmul))))
  ([Sgens Smul Tgens Tmul conj-fn-bundle] ; ALL EMBEDDINGS UP TO CONJUGATION
   (let [{mSgens :gens mSmul :mul :as mS} (gentab Sgens Smul)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings-distinct mSgens
                               mSmul
                               (index-period-matched mS Tgens Tmul)
                               Tmul
                               conj-fn-bundle)))))

(defn embeddings
  "All embeddings of source semigroup into target induced by the possible
  images of the generators."
  [Sgens Smul tgs Tmul]
  (trace "Number of targets:" (vec (map count tgs)))
  (let [solution? (fn [[n _]] (= n (count Sgens))) ;n - #generators, m - morphs
        generator (fn [[n m :as v]]
                    (if (solution? v)
                      []
                      (let [f (fn [g]
                                (add-gen-and-close m (nth Sgens n) g
                                                   (take (inc n) Sgens)
                                                   Smul Tmul))
                            result (r/reduce
                                    #(conj %1 [(inc n) %2])
                                    []
                                    (r/filter #(apply distinct? (vals %))
                                              (r/remove nil?
                                                        (r/map f
                                                               (nth tgs n)))))]
                        (trace "Generators:" n
                              " Partial morphs:" (count m)
                              " Targets: " (count (nth tgs n))
                              " Realized:" (count result))
                        result)))]
    (map second (ptree-search [[0 {}]]
                              generator
                              solution?))))

(defn distinct-up-to-f
  "Classifies the elements of coll by function f and keeps only
  a representative element from each class."
  [f coll]
  (map first (vals (group-by f coll))))

(defn morphisms-up-to-conjugation
  "Returns the distinct morphs up to conjugation. First checking by the equality
  of the image set, then by its conjugacy class representative."
  [morphs setconjrep]
  (->> morphs
       (distinct-up-to-f (fn [m] (set (vals m))))
       (distinct-up-to-f (fn [m] (setconjrep (vals m))))))

(defn new-generator-conjreps
  "Finds the possible target generators up to conjugation. For the very first
  generator it chooses conjugacy class representatives. "
  [phi n Sgens tgs
   {repconj :conjrep setconjrep :setconjrep conj-conj :conjconj}]
  (if (zero? n)
    (set (map repconj (first tgs)))
    (let [gens (mapv phi (take n Sgens))
          partconj (reduce conj-conj ; for not computing minconjugators always
                           (conj-conj (first gens))
                           (rest gens))
          conjed_seqs (r/map first
                             (r/map (partial conj-conj partconj)
                                    (nth tgs n)))
          ;we need to keep distinct generator sets
          selected_seqs (distinct-up-to-f setconjrep conjed_seqs)]
      (into #{} (r/map last selected_seqs))))) ;set does not work here

(defn embeddings-distinct
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul tgs Tmul conj-fn-bundle]
  (let [solution? (fn [[n _]] (= n (count Sgens))) ;n #generators, phi morphs
        generator (fn [[n phi :as v]]
                    (if (solution? v)
                      []
                      (let [ngens (new-generator-conjreps phi n Sgens tgs
                                                          conj-fn-bundle)
                            check-gen (fn [newmorphs ngen]
                                        (let [gens (take (inc n) Sgens)
                                              nmorph (add-gen-and-close
                                                      phi
                                                      (nth Sgens n)
                                                      ngen
                                                      gens
                                                      Smul
                                                      Tmul)]
                                          (if (and (coll? nmorph)
                                                   (apply distinct? ;iso?
                                                          (vals nmorph)))
                                            (conj newmorphs [(inc n) nmorph])
                                            newmorphs)))
                            result (reduce check-gen [] ngens)]
                        (trace (count phi) "elts in phi,"
                               (count ngens) "targets for gen" n ","
                               (count result) "realized" (mem-info))
                        result)))
        morphs (map second (ptree-search [[0 {}]]
                                         generator
                                         solution?))]
    (trace (count morphs) "morphisms found." (mem-info))
    (morphisms-up-to-conjugation morphs (:setconjrep conj-fn-bundle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-gen-and-close
  "Add a new generator and close the Cayley-graph.
  There are two phases for adding a new generator
  1. we multiply everything (including itself by the new generator)
  2. for any new elements generated we go through with all generators so far"
  [phi gen phiofgen gens Smul Tmul]
  (let [ephi (conj phi [gen phiofgen])
        res (sys-mul ephi (keys ephi) [gen] Smul Tmul)] ;phase 1
    (when-not (nil? res)
      (loop [phi (:phi res) newelts (conj (:new res) gen)]
        (if (empty? newelts)
          phi
          (let [res (sys-mul phi newelts gens Smul Tmul)]
            (when-not (nil? res)
              (recur (:phi res) (:new res)))))))))

(defn sys-mul
  "Systematic right multiplication of elts by gens and collecting new elements.
  Elts and gens are all in phi already. Phi is being built along the way
  but new elements need proper extension later, that's why they are collected."
  [phi elts gens Smul Tmul]
  (loop [phi phi
         newelts [] ;newly generated elements
         pairs (for [a elts b gens] [a b])] ;possible a,b pairs for products
    (if (empty? pairs)
      {:phi phi :new newelts}
      (let [v (first pairs)
            p (new-mapping phi (first v) (second v) Smul Tmul)]
        (cond (nil? p) p
              (empty? p) (recur phi
                                newelts
                                (rest pairs))
              :else (recur (conj phi p)
                           (conj newelts (first p))
                           (rest pairs)))))))

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
