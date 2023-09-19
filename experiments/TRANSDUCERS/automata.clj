;;automata related functions

;;TODO write a function that checks the io-pairs for contradicting pairs
;; like the same word requiring two different outputs

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

(defn search
  "It returns the the coords (the nested keys) in the trie and the position in the
  word where the search fails. If the word is fully matched, it returns :matched."
  [trie word]
  (loop [coords [0], pos 0] ;start with the first entry and first symbol 
    (let [stored (get-in trie coords) ;try to get them
          letter (get word pos)]
      ;(println "stored " stored  "letter " letter)
      (cond
        (nil? letter) :matched ;no more letters in word
        (nil? stored) [coords, pos] ;no more entries in trie, the word is longer
        :else (cond
                ;vector means branching, but we need to find the right branch
                (vector? stored) (let [branch (filter
                                               (fn [i] (= letter (first (get stored i))))
                                               (range (count stored)))]
                                   (if (empty? branch)
                                     [coords pos] ;mismatch if no good branch found
                                     ;otherwise continue matching in the right branch
                                     (recur (conj coords (first branch) 0) pos)))
                ;letter matches, keep going
                (= stored letter) (recur
                                   (update coords (dec (count coords)) inc)
                                   (inc pos))
                ;different symbol so return the mismatch positions
                :else [coords, pos])))))

(defn insert-at
  [trie word coords pos]
  (let [suffix (vec (drop pos word))
        stored (get-in trie coords)]
    (if (vector? stored)
      ;adding a new branch by adding a new vector
      (update-in trie coords (fn [v] (conj v suffix)))
      ;we don't have a vector so we may need to create one
      (let [location (butlast coords)
            the_vec_we_are_in (if (empty? location)
                                trie
                                (get-in trie location))]
        ;(println "we are in " the_vec_we_are_in) 
        (let [[pref suff] (split-at (last coords) the_vec_we_are_in)
              newnode (if (empty? suff)
                        (into (vec pref) suffix) ;when no need to branch
                        (conj (vec pref) [(vec suff) suffix]))]
          (if (empty? location)
            newnode
            (update-in trie location (fn [v] newnode))))))))

(defn insert [trie word]
  (let [result (search trie word)]
    (if (= result :matched)
      trie
      (let [ [coords pos] result]
        (insert-at trie word coords pos)))))

(defn retrieve
  ([trie] (retrieve trie [] []))
  ([trie so-far result]
   (let [elts (take-while (fn [x] (not (vector? x))) trie)
         new-so-far (into so-far elts)]
     (if (= (count trie) (count elts))
       (conj result new-so-far)
       (reduce
        (fn [r v]  (g v new-so-far r))
        result
        (last trie))))))

(def words ["eruption" "erudite" "education" "oogabooga" "cat" "dog" "elephant"
            "banana" "mango" "a" "landwirtschaftfachausstellung"])

(def common-words ["the","of","and","a","to","in","is","you","that","it","he","was","for","on","are","as","with","his","they","I","at","be","this","have","from","or","one","had","by","word","but","not","what","all","were","we","when","your","can","said","there","use","an","each","which","she","do","how","their","if","will","up","other","about","out","many","then","them","these","so","some","her","would","make","like","him","into","time","has","look","two","more","write","go","see","number","no","way","could","people","my","than","first","water","been","call","who","oil","its","now","find","long","down","day","did","get","come","made","may","part"])

(def trie )


(defn check
  [words]
  (let [trie (reduce insert words)]
    (= (set words)
       (set (map (partial apply str) (retrieve trie))))))