(ns kigen.cayley-graph
  "Computing Cayley-graphs.")

;; elts - a set of elements
;; afs - operations that act on elts, i.e. functions: elt -> elt
(defn cayley-graph
  "Unlabelled Cayley graph of elements elts by action functions afs.
  Returns a map: element -> image set (successors) of element"
  [elts afs]
  (into {} (for [x elts]
             [x (set (for [o afs] (o x)))])))

;;clj-me.cgrand.net/2013/03/18/tarjans-strongly-connected-components-algorithm/
;;https://gist.github.com/cgrand/5188919
;;en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

;;env is a map containing:
;; 1. a stack of nodes in the order of search
;; 2. the current list of SCCs
;; 3. key-value pairs: from nodes to time of discovery (stack length)
;;                                   or oldest ancestor's value,
;;                                   or nil,
;;EXAMPLE
;;kigen.core=> (kigen.orbit/scc #{:1 :2 :3} {:1 #{:2 } :2 #{:3} :3 #{:2}})
;;#{#{:2 :3} #{:1}}
(defn scc
  "Returns the strongly connected components of a graph specified by its nodes
  and a successor function succ-func from node to nodes."
  [nodes succ-func]
  (letfn [(strongconn [env node]
;            (println "strogconn" env node)
            (if (contains? env node)
              env ;nothing to do, the node is already visited
              (let [stack (:stack env)
                    n (count stack) ;n is the time of discovery for node
                    env (assoc env node n :stack (conj stack node))
                    f (fn [env succ]
 ;                       (println "f" env succ "n=" n)
                       (let [env (strongconn env succ)]
                         (assoc env
                                node
                                (min (or (env succ) n) (env node)))))
                    env (reduce f env (succ-func node))]
                (if (= n (env node)) ; no link below, an SCC found
                  (let [nodes (:stack env)
                        scc (set (take (- (count nodes) n) nodes))
                        env (reduce #(assoc %1 %2 nil) env scc)] ;clear lengths
;                    (println "scc found" scc)
                    (assoc env :stack stack :sccs (conj (:sccs env) scc)))
                  env))))]
    (:sccs ;getting the result from the environment
     (reduce strongconn
             {:stack () :sccs #{}} ;initial environment
             nodes))))
