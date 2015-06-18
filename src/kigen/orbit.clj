(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare orbit
         alternating-orbit
         orbit-graph)

;; seed - elements to act on
;; funcs - functions that produce a new element applied to an element
;; TODO temporarily dispatching to different functions
(defn orbit
  ([seed funcs] (alternating-orbit seed (cycle [funcs])))
  ([seed gens action] (:orbit (orbit-graph seed gens action))))

;; seed - elements to act on
;; funcs-seq - sequence of function colls
;; in each step we may apply different set of functions
(defn alternating-orbit
  [seed funcs-seq]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set  seed)]
         total (first o)
         funcs-seq funcs-seq]
    (let [newelts  (for [x (last o) f (first funcs-seq)] (f x))
          newset (reduce into #{} newelts)
          diff (difference newset total)]
      (if (empty? diff)
        total
        (recur (conj o diff)
               (into total diff)
               (rest funcs-seq))))))

(defn orbit-graph
  [seed gens action]
  (let [og {:seed (set seed) :graph {seed {}} :orbit #{seed}}
        funcs (for [g gens] #(action % g))
        indxs (range 0 (count funcs))]
    (loop [frontier [seed] og og]
      (let [frontier (for [x frontier i indxs] [((nth funcs i) x) {i x}])
            diff (filter (fn [[x]] (not (contains? (:orbit og) x))) frontier)
            nodes (map first diff)]
        (if (empty? nodes)
          (conj og [:gens gens])
          (recur nodes {:seed seed
                        :orbit (into (:orbit og) nodes)
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
  (trace target (:graph (orbit-graph source gens action))))

;; stack: node and operation pairs, the work that still needs to be done
(defn dfs
  [start ops]
  (let [opf #(for [op ops] [% op])] ;generating node-op pairs to be pushed
    (loop [stack (into [] (opf start))
           coll #{start}]
      (if (empty? stack)
        coll
        (let [[e op] (peek stack)
              ne (op e)
              nstack (pop stack)]
          (if (contains? coll ne)
            (recur nstack coll)
            (recur (into nstack (opf ne)) (conj coll ne))))))))

(defn dfs2
  [start ops]
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
    (f {::stack (into [] (opf start))
        ::orbit #{start}})))

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
