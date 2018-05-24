# Racets -- Faceted Execution in Racket

An implementation of faceted execution in the Racket programming
language. Faceted execution is a technique that dynamically tracks
information flow to ensure privileged data does not leak to
unprivileged outputs. A rough analogy might be a more powerful version
of taint tracking.

For more information about Racket, you can read the following papers:

- Multiple Facets for Dynamic Information Flow. Austin et al. (POPL
  '12) https://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf
  
- Faceted Execution of Policy-Agnostic Programs. Austin et al. (PLAS
  '13) https://users.soe.ucsc.edu/~cormac/papers/plas13.pdf

You may also want to look at the
[Jeeves](https://projects.csail.mit.edu/jeeves/about.php) programming
language, which uses similar concepts to Racets.

Racets extends the Racket programming language to allow writing
programs that compute with secret values. Racets does this by
providing a `#lang` language built on top of Racket.
