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

;do a mechanism that goes through the trie
(defn traverse
  [trie coords]
  (let [parent (get-in trie (butlast coords))
        pos (last coords)
        thing (get-in trie coords)]
    (if (vector? thing)
      (doseq [i (range (count thing))]
        (traverse trie (into coords [i 0]))) 
      (when (< pos (count parent))
        (do
          (println coords thing)
          (traverse trie (update coords (dec (count coords)) inc)))))))