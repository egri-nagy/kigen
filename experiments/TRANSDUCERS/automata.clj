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

(defn recognizer
  "Returns a recognizer FA for the trie."
  ([trie] (recognizer trie 0 {} #{:accept}))
  ([trie state delta acceptors]
   (let [elts (take-while (fn [x] (not (vector? x))) trie)
         (reduce (fn [])
                 delta)]
     (if (= (count trie) (count elts))
       (conj result new-so-far)
       (reduce
        (fn [r v]  (retrieve v new-so-far r))
        result
        (last trie))))))