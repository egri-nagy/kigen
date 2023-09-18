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
  "It returns the the coords (the nested keys) in the trie where the search
   fails. If the word is fully matched, it returns :matched."
  [trie word]
  (loop [coords [0], pos 0] ;start with the first entry and symbol
    ;(println coords pos)
    (let [stored (get-in trie coords) ;try to get them
          letter (get word pos)]
      ;(println stored)
      (cond
        (nil? letter) :matched ;no more letters in word
        (nil? stored) [coords, pos] ;no more entries in trie
        :else (cond
                ;vector means branching, but we need to find the right branch
                (vector? stored) (let [branch (filter
                                               (fn [i] (= letter (first (get stored i))))
                                               (range (count stored)))]
                                   (if (empty? branch)
                                     [coords pos] ;mismatch if no good branch
                                     ;otherwise continue matching in the right branch
                                     (recur (conj coords (first branch) 0) pos)))
                ;letter matches, keep going
                (= stored letter) (recur
                                   (update coords (dec (count coords)) inc)
                                   (inc pos))
                ;different symbol so return the mismatch positions
                :else [coords, pos])))))

(defn insert
  [trie word coords pos]
  (let [suffix (vec (drop pos word))
        stored (get-in trie coords)]
    (if (vector? stored)
      (update-in trie coords (fn [v] (conj v suffix)))
      (let [location (butlast coords)
            the_vec_we_are_in (if (empty? location)
                                trie
                                (get-in trie location))
            [pref suff] (split-at (last coords) the_vec_we_are_in)
            newnode (conj (vec pref) [(vec suff) suffix])]
        (if (empty? location)
          newnode
          (update-in trie location (fn [v] newnode)))))))