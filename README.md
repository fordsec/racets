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

# Codebase

The main implemenation of the language lives in
`racets-mlang.rkt`. Many macros are provided that implement faceted
execution for Racet. `facets.rkt` contains a library of operations on
facets that are used to construct and manage facets.

# Paper

Lives in the `/paper` directory, and may be built using the Makefile
provided.

# Case study: Battleship webapp

As a case study in how Racets can be used to perform policy-agnostic
programming, we have implemented a web-based game of Battleship in
Racket. The source for this game lives in `case-study.rkt`, and is
written using Racets macros through Racket's `#lang reader`
syntax. The game uses Racket's `web-server` package, which may need to
be installed through `raco pkg install web-server`.

To run the webapp, use:

    racket case-study.rkt

The webapp has four routes:
- `/player1/<id>`: reveal's player 1's game board when `id` is
  `player1`, otherwise shows the empty board.

- `/player2strike/x,y`: makes a strike on player 2's board game at the
  coordinate `(x,y)`.

- `/player2/<id>`, `/player1strike/x,y`: do the same things for player
  2.
