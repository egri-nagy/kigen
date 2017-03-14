[![Build Status](https://travis-ci.org/egri-nagy/kigen.svg?branch=master)](https://travis-ci.org/egri-nagy/kigen)
# kigen

Computational Semigroup Theory Software System written in [Clojure](https://clojure.org/)

To main purpose here is to "shadow" other computer algebra packages in semigroup theory, i.e. to serve as a "clean room" implementation recomputation tool. For mathematical research, where proofs or formal verification of the methods are not (yet) available, computing the same experiments with different algorithms implemented in different programming languages running on different architectures is a way to establish the correctness of computational results.

The shadowed semigroup packages are available in the [GAP](https://www.gap-system.org) computer algebra system. Namely,

1. [Semigroups](https://gap-packages.github.io/Semigroups/),
2. [SgpDec](https://gap-packages.github.io/sgpdec/),
3. [SubSemi](https://gap-packages.github.io/subsemi/).

As of the 17.03.14 Pi Day release, kigen includes
  * calculating the skeleton for holonomy decompositions of transformation semigroups
  * enumerating all subsemigroups and independent generating set of a semigroup using its multiplication table
  * backtrack search for relational morphisms, divisions, homomorphism and isomorphisms by multiplication tables
  * finding embeddings and isomorphisms of semigroups given by generating sets

kigen - origin in Japanese
