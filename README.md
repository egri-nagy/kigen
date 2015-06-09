# kigen
General implementation of diagram semigroups based on partitioned binary relations (as defined in http://arxiv.org/abs/1102.0862).

Goals:

1. To serve as a recomputation tool for existing diagram semigroup packages, e.g. Semigroups, SgpDec, SubSemi of GAP.
2. To be minimalistic by implementing only the most general diagram representation, the partitioned binary relations.
3. Establish more transparent correctness of semigroup algorithms.
4. Have fun with a great programming language: Clojure.
5. Incidentally, once in Clojure, we can use parallelism tools.

Non-Goals:

1. Speed - by definition no optimized representations for transformations, permutations. 
2. Replacing existing  semigroup packages.

kigen - origin in Japanese
