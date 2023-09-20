(ns kigen.transducer.trie)

(defn search
  "Searching a word in a trie, reporting the location of mismatch if the word
   is not in the trie.
   It returns the the coords (the nested keys) in the trie and the position in
   the word where the search fails. If the word is fully matched, it returns
   :matched."
  [trie word]
  (loop [coords [0], pos 0] ;start with the first entry and first symbol 
    (let [entry (get-in trie coords) ;try to get them
          letter (get word pos)]
      (cond
        (nil? letter) :matched ;no more letters in word
        (nil? entry) [coords, pos] ;no more entries in trie, the word is longer
        :else (cond
                (vector? entry) ;vector means branching
                  ;;which one is the right branch?
                (let [branch (filter
                              (fn [i] (= letter (first (get entry i))))
                              (range (count entry)))] ;branch indices
                  (if (empty? branch)
                    [coords pos] ;mismatch if no good branch found
                      ;;otherwise continue matching in the right branch
                    (recur (conj coords (first branch) 0) pos)))
                ;letter matches, keep going, next in entry, and next symbol
                (= entry letter) (recur
                                  (update coords (dec (count coords)) inc)
                                  (inc pos))
                ;different symbol so return the mismatch positions
                :else [coords, pos])))))

(defn insert-at
  "Insert a word into the trie at the location given by search."
  [trie word [coords pos]]
  (let [suffix (vec (drop pos word)) ;we only insert this suffix
        entry (get-in trie coords)]
    (if (vector? entry)
      ;adding a new branch by adding a new vector
      (update-in trie coords (fn [v] (conj v suffix)))
      ;we don't have a vector so we may need to create one
      (let [location (butlast coords) ;where we make the change
            parent (get-in trie location)
            [pref suff] (split-at (last coords) parent)
            newnode (if (empty? suff)
                      (into (vec pref) suffix) ;when no need to branch
                      (conj (vec pref) [(vec suff) suffix]))]
        (if (empty? location)
          newnode
          (update-in trie location (constantly newnode)))))))

(defn insert
  "Inserts a word into the trie. It takes care of finding the location of
   mismatch if any."
  [trie word]
  (let [match-result (search trie word)]
    (if (= match-result :matched)
      trie
      (insert-at trie word match-result))))

(defn build-trie
  "Builds a trie form a collection of words"
  [words]
  (reduce insert words))

(defn retrieve
  "Returns all the words stored in the trie."
  ([trie] (retrieve trie [] []))
  ([trie so-far result]
   (let [elts (take-while (fn [x] (not (vector? x))) trie)
         new-so-far (into so-far elts)]
     (if (= (count trie) (count elts))
       (conj result new-so-far)
       (reduce
        (fn [r v]  (retrieve v new-so-far r))
        result
        (last trie))))))
