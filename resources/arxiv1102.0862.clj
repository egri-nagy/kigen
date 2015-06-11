;; example from http://arxiv.org/abs/1102.0862
(def alpha {:dom #{1 2 3 4 5 6 7} :cod #{8 9 10 11 12 13 14 15 16}
            1 #{9} 2 #{} 3 #{4} 4 #{4} 5 #{} 6 #{13} 7 #{14}
            8 #{8} 9 #{1 10} 10 #{11} 11 #{} 12 #{2,12} 13 #{6}
            14 #{7} 15 #{16} 16 #{15}})

(def beta {:dom #{1 2 3 4 5 6 7 8 9} :cod #{10 11 12 13}
           1 #{} 2 #{10} 3 #{3} 4 #{5} 5 #{4 11} 6 #{6} 7 #{} 8 #{12} 9 #{}
           10 #{2} 11 #{} 12 #{8} 13 #{9}})

(def alphabeta {:dom #{1 2 3 4 5 6 7} :cod #{8 9 10 11}
                1 #{8} 2 #{} 3 #{4} 4 #{4} 5 #{} 6 #{6} 7 #{}
                8 #{1 2 9} 9 #{} 10 #{} 11 #{10}})