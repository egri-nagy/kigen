(require '[kigen.transducer.trie :refer :all])
;;automata related functions

;;TODO write a function that checks the io-pairs for contradicting pairs
;; like the same word requiring two different outputs

(def stop \⏹)

(defn proper-prefixes
  "All proper prefixes of the given word, starting from the empty word."
  [coll]
  (butlast (reductions conj [] coll)))

(def i-o [["aa" :as]
          ["bb" :bs]
          ["ab" :mixed]
          ["ba" :mixed]])

(defn recognizer-inputs
  "Takes transducer i-o pairs and separates them into recognizer
   inputs."
  [io-pairs]
  (update-vals (group-by second io-pairs)
               (partial map first)))

(defn firsts-in-trie
  "Returns a set of the elements that appear the first positions in the
   branches."
  [trie]
  (cond
    (empty? trie) #{}
    (vector? (first trie)) (into #{} (map first (first trie)))
    :else #{(first trie)}))

;traversing the trie, collecting stored things and their coordinates
(defn traverse
  [trie]
  (let [stopper [(count trie)]]
    (loop [ coords [0] bag [] counter 0] 
      (let [location (vec (butlast coords))
            parent (get-in trie location)
            pos (last coords)
            thing (get-in trie coords)
            ncoords (cond
                      (vector? thing)  (conj coords 0)
                      (< pos (count parent)) (update coords
                                                     (dec (count coords))
                                                     inc)
                      (nil? thing) (update location
                                           (dec (count location))
                                           inc))
            not-real? (or (nil? thing) (vector? thing)) 
            nbag (if not-real?
                   bag
                   (conj bag ["coords:" coords "thing:" thing counter]))
            ncounter (if not-real? counter (inc counter))] 
        (if (= stopper ncoords)
          nbag
          (recur ncoords nbag ncounter))))))