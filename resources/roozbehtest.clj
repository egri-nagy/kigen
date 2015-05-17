;;test example from Roozbeh from his Mathematica implementation
(def RHa {:dom #{1 2 3 4}
        :cod #{5 6 7 8}
        1 #{} 2 #{7} 3 #{} 4 #{}
        5 #{} 6 #{1,7} 7 #{} 8 #{7,4}})

(def RHb {:dom #{1 2 3 4}
        :cod #{5 6 7 8}
        1 #{} 2 #{} 3 #{2 4} 4 #{8}
        5 #{2} 6 #{} 7 #{} 8 #{}})

(def RHab {:dom #{1 2 3 4}
        :cod #{5 6 7 8}
        1 #{} 2 #{1 4} 3 #{} 4 #{}
        5 #{1 4} 6 #{} 7 #{} 8 #{}})
