(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare orbit
         bfs
         orbit-graph
         actions)

;; seed - elements to act on
;; funcs - functions that produce a new element applied to an element
;; TODO this is just temporary dispatching
(defn orbit
  [seed gens action] (bfs [seed] (actions gens action)))

(defn actions
  "Creating actions (as functions) from operators and a function for
  describing how an operator acts"
  [operators action]
  (for [o operators] #(action % o)))

;; BREADTH-FIRST SEARCH
;; seeds - elements to act on
;; afs - action functions
(defn bfs
  [seeds afs]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set  seeds)], total (first o)]
    (let [newset (set (for [x (last o) f afs] (f x)))
          diff (difference newset total)]
      (if (empty? diff)
        total
        (recur (conj o diff) (into total diff))))))

;; DEPTH-FIRST SEARCH
;; stack: node and operation pairs, the work that still needs to be done
(defn dfs
  [seeds ops]
  (let [opf #(for [op ops] [% op])] ;generating node-op pairs to be pushed
    (loop [stack (into [] (mapcat opf seeds))
           coll (set seeds)]
      (if (empty? stack)
        coll
        (let [[e op] (peek stack)
              ne (op e)
              nstack (pop stack)]
          (if (contains? coll ne)
            (recur nstack coll)
            (recur (into nstack (opf ne)) (conj coll ne))))))))

;;another variant - environment style
(defn dfs2
  [seeds ops]
  (letfn [(opf [t] (for [op ops] [t op])) ;generating node-op pairs to be pushed
          (f [env]
            (let [stack (::stack env)
                  orbit (::orbit env)]
              (if (empty? stack)
                orbit
                (let [[e op] (peek stack)
                      ne (op e)
                      nstack (pop stack)]
                  (if (contains? orbit ne)
                    (recur (assoc env ::stack nstack))
                    (recur (assoc env
                                  ::stack (into nstack (opf ne))
                                  ::orbit (conj orbit ne))))))))]
    (f {::stack (into [] (mapcat opf seeds))
        ::orbit (set  seeds)})))


;;ORBIT-GRAPH
(defn orbit-graph
  [seed fs]
  (let [indxs (range 0 (count fs))]
    (loop [frontier [seed] og {:graph {seed {}} :orbit #{seed}}]
      (let [elts (for [x frontier i indxs] [((nth fs i) x) {i x}])
            diff (filter (fn [[x]] (not (contains? (:orbit og) x))) elts)
            newfront (map first diff)]
        (if (empty? newfront)
          og
          (recur newfront {:orbit (into (:orbit og) newfront)
                           :graph (into (:graph og) diff)}))))))

(defn trace
  "Tracing a path to an element in the orbit graph"
  [e og]
  (drop 1 (rseq (loop [e e r []]
                  (if (nil? e)
                    r
                    (let [[[k v]] (seq (og e))]
                      (recur v (conj r k))))))))

;;immensely wasteful, but will do the job in the beginning
(defn geodesic
  [source target gens action]
  (trace target (:graph (orbit-graph source (actions gens action)))))


;;dfs with a set-valued operator
(defn sdfs
  [start op]
  (loop [stack (vec start)
         coll (set start)]
    (if (empty? stack)
      coll
      (let [e (peek stack)
            nes (op e)
            nstack (pop stack)
            c (filter #(not (contains? coll %)) nes)]
        (recur (into nstack c)
               (into coll c))))))


;; the operations should be supplied (can be left or right action)
;; elts - a set of elements
;; ops - operations that act on elts, i.e. functions: elts -> elts
(defn cayley-graph
  ;;  TODO the graph is not labelled
  [elts ops]
  (into {} (for [x elts]
             [x (set (for [o ops] (o x)))])))

;;clj-me.cgrand.net/2013/03/18/tarjans-strongly-connected-components-algorithm/
;;en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

;;env is a map from nodes to stack length or nil,
;;nil means the node is known to belong to another SCC
;; ::stack  the current stack
;; ::sccs   the current set of SCCs
(defn scc
  "Returns the strongly connected components of a graph specified by its nodes
  and a successor function succs from node to nodes."
  [nodes succs]
  (letfn [(strongconn [env node]
            (if (contains? env node)
              env ;nothing to do, the node is already visited
              (let [stack (::stack env)
                    n (count stack)
                    env (assoc env node n ::stack (conj stack node))
                    env (reduce (fn [env succ]
                                  (let [env (strongconn env succ)]
                                    (assoc env
                                           node
                                           (min (or (env succ) n) (env node)))))
                                env (succs node))]
                (if (= n (env node)) ; no link below, an SCC found
                  (let [nodes (::stack env)
                        scc (set (take (- (count nodes) n) nodes))
                        env (reduce #(assoc %1 %2 nil) env scc)] ;clear lengths
                    (assoc env ::stack stack ::sccs (conj (::sccs env) scc)))
                  env))))]
    (::sccs ;getting the result from the environment
     (reduce strongconn
             {::stack () ::sccs #{}} ;initial environment
             nodes))))
