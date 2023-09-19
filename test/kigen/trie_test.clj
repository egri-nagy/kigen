(ns kigen.trie-test
  (:require [clojure.test :refer :all]
            [kigen.transducer.trie :refer :all]))

(def words ["eruption" "erudite" "education" "oogabooga" "cat" "dog" "elephant"
            "banana" "mango" "a" "landwirtschaftfachausstellung"])

(def common-words ["the","of","and","a","to","in","is","you","that","it","he","was","for","on","are","as","with","his","they","I","at","be","this","have","from","or","one","had","by","word","but","not","what","all","were","we","when","your","can","said","there","use","an","each","which","she","do","how","their","if","will","up","other","about","out","many","then","them","these","so","some","her","would","make","like","him","into","time","has","look","two","more","write","go","see","number","no","way","could","people","my","than","first","water","been","call","who","oil","its","now","find","long","down","day","did","get","come","made","may","part"])

(def common-words-terminated
  (map (fn [w] (str w \@)) common-words))

(defn check
  [words]
  (let [trie (build-trie words)]
    (= (set words)
       (set (map (partial apply str) (retrieve trie))))))

(deftest test-trie-retrieval
  (testing "Testing a trie for retrieving all words stored."
    (is (check words))
    (is (check common-words-terminated))))