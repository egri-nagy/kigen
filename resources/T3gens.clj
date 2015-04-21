(def transposition {:dom #{1 2 3} :cod #{4 5 6} 1 #{5} 2 #{4} 3 #{6} 4 #{} 5 #{} 6 #{}})
(def cyclic {:dom #{1 2 3} :cod #{4 5 6} 1 #{5} 2 #{6} 3 #{4} 4 #{} 5 #{} 6 #{}})
(def collapser {:dom #{1 2 3} :cod #{4 5 6} 1 #{4} 2 #{4} 3 #{6} 4 #{} 5 #{} 6 #{}})

(def T3gens [transposition cyclic collapser])
