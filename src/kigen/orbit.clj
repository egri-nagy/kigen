(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare orbit
         alternating-orbit
         orbit-graph)

;; seed - elements to act on
;; funcs - functions that produce a new element applied to an element
(defn orbit
  [seed funcs]
  (alternating-orbit seed (cycle [funcs])))

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
  (let [og {:seed (set seed)
            :gens gens
            :graph {seed {}}
            :orbit #{seed}}
        funcs (for [g gens] #(action % g))
        indxs (range 0 (count funcs))]
    (loop [frontier [seed] og og]
      (let [frontier (for [x frontier i indxs] [((nth funcs i) x) {i x}])
            diff (filter (fn [[x]] (not (contains? (:orbit og) x))) frontier)
            nodes (map first diff)]
        (if (empty? nodes)
          og
          (recur nodes
                 {:seed seed
                  :gens (:gens og)
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

;; stack: node and operation pairs, the work that still needs to be done
(defn dfs
  [start ops]
  (let [opf #(for [op ops] [% op])] ;generating node-op pairs to be pushed
    (loop [stack (into [] (opf start))
           coll #{}]
      (if (empty? stack)
        coll
        (let [[e op] (peek stack)
              ne (op e)
              nstack (pop stack)]
          (if (contains? coll ne)
            (recur nstack coll)
            (recur (into nstack (opf ne)) (conj coll ne))))))))

(defn digraph
  [nodes ops]
  (into {} (for [n nodes]
           [n (vec (for [o ops] (o n)))])))

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
