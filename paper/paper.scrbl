#lang scribble/acmart @acmlarge @review @natbib
@(require
  (only-in scribble/manual
           racketblock
           codeblock)
  scriblib/bibtex
  latex-utils/scribble/math)

@title{Racets: Embedding Policy-Agnostic Programming in Racket}
@author["Kristopher Micinski"
        #:affiliation @affiliation[#:institution
                                   @institution[#:departments (list @institution{Department of Computer Science})]{Haverford College}
                                   #:city "Haverford"
                                   #:state "Pennsylvania"]
        #:email "kris@cs.haverford.edu"]
@author["Zhanpeng Wang"
        #:affiliation @affiliation[#:institution
                                   @institution[#:departments (list @institution{Department of Computer Science})]{Haverford College}
                                   #:city "Haverford"
                                   #:state "Pennsylvania"]
        #:email "zwang10@haverford.edu"]
@author["Thomas Gilray"
        #:affiliation @affiliation[#:institution
                                   @institution[#:departments (list @institution{Department of Computer Science})]{University of Alabama}
                                   #:city "Birmingham"
                                   #:state "Alabama"]
        #:email "gilray@uab.edu"]

@abstract{lorem ipsum...}

@(define (code item)
   (tt item))

@(define (facet label left right)
   (m "\\langle " label " ~~?~~ " left ":" right "\\rangle"))

@section[#:tag "intro"]{Introduction}

As our systems become more interconnected, they consume an
ever-growing amount of data.  System designers communicate to users
how their data is used via a @emph{privacy policy}. Unfortunately,
implementing these policies correctly is challenging: users often have
partial control over the policy (e.g., whether their phone number is
publicly visible or private) and policies change frequently (e.g.,
after new laws are passed). As policies evolve, developers face
massive (re)engineering efforts to ensure that implementations match
the espoused policy.

@emph{Policy-agnostic programming} is a linguistic paradigm that
decouples the implementation of privacy policies from the code that
operates on sensitive data. This frees developers to write programs as
they would normally, without inserting logic to manage the policy
directly into application code. Instead, data is labeled with its
policy as it enters the system, and propagates through the program as
computation progresses. Policy-agnostic programming is often
implemented using faceted execution, a dynamic information-flow
monitor encoding sensitive values as @emph{facets}: decision trees
specifying different views of that data. For example, the facet
@(facet @italic{Alice} @code{#t} @code{#f}) represents a value that
should appear to Alice as @code{#t} and to everyone but Alice as
@code{#f}.

Faceted execution propagates facets by extending core linguistic
primitives (such as function application). For example, consider the
application @code{(not x)} where @code{x} is @(facet @italic{Alice}
@code{#t} @code{#f}). The programmer intends the call to @code{not} to
be operating on a @code{boolean?}, but because @code{x} is a facet
this call produces @code{#f} instead. The correct way to interpret
function application on facets is to distribute the application
through each branch so that we end up with @(m (facet @italic{Alice}
@code{(not #t)} @code{(not #f)}) "=" (facet @italic{Alice} @code{#f}
@code{#t})). Other core forms (such as @code{if}, @code{cond}, etc...)
require similar changes.

The core linguistic changes required by faceted execution have led to
relatively inelegant and heavyweight implementations thusfar. For
example, Flanagan et. al (who first presented faceted execution)
extended the JavaScript runtime to account for faceted
values. Similarly, the Jeeves programming language does (what does it
do?). By contrast, Racket's highly-expressive macro system allows even
core forms (such as @code{#%app}) to be redefined with relative ease.

In this paper we present Racets, an implementation of policy-agnostic
programming in Racket via macros. Racets provides facilities for
creating policies and faceting secure data with those policies. Racets
also extends several core forms in Racket to work with faceted
values (our implementation is detailed in @Secref{implementation}). We
have measured the overhead of Racets in several microbenchmarks, and
have used Racets to implement a small server-based
board-game (detailed in @Secref{evaluation}). We see Racets as a
promising prototype for policy-agnostic programming in Racket, and
conclude with several future directions in @Secref{conclusion}.

@section[#:tag "overview"]{Overview of Faceted Execution}

To introduce our setting we present the implementation of Battleship,
a small guessing-based board game, in Racets (this section presents a
distilled version of our case study in @Secref{case-study}). In this
game each player has a grid-based board on which they place tiles (or
``ships''). The players hide their boards from each other as play
progresses in rounds. Each turn a player guesses the position of a
tile on the other player's board. If the guess is successful that tile
is removed from the board. Play ends once one player's board has no
remaining tiles, at which point that player loses.

Our program begins by specifying that it should use our Racets macros,
rather than plain Racket:

@;{@codeblock|{
  #lang reader "racets.rkt"
}|}

We implement game boards as lists of cons cells representing the
@(m "(x,y)") coordinates of ships on the board. Board creation is
simply the empty list, and adding a piece is done via @code{cons}:

@codeblock|{
  (define (makeboard) '())
  (define (add-piece board x y) (cons (cons x y) board))
}|

Next we define @code{mark-hit}, which takes a player's board and
removes a piece if the guessed coordinate is present. We return a pair
of the updated board and a boolean indicating whether the guess was a
hit:

@codeblock|{
  (define (mark-hit board x y)
    (if (null? board)
        (cons board #f)
        (let* ([fst (car board)]
               [rst (cdr board)])
          (if (and (= (car fst) x)
                   (= (cdr fst) y))
              (cons rst #t)
              (let ([rst+b (mark-hit rst x y)])
                (cons (cons fst
                            (car rst+b))
                      (cdr rst+b)))))))
}|

Although @code{mark-hit} will operate on sensitive data (the game
boards), it is written without any special machinery to maintain the
secrecy of @code{board}. Protecting data w.r.t. policies is instead
handled automatically and implicitly by a runtime monitor.  When Alice
and Bob want to play a game, they both create a label to protect their
data. A label is a predicate that takes an argument and, if it returns
true, reveals a secret. Alice's label is used to annotate whatever
data she wants to be kept secret. Supposing Alice chooses to be player
1, she will use the following label:

@codeblock|{
  (define alice-label (let-label alice (lambda (x) (= 1 x)) alice))
}|

Bob would use a similar label (but with 2 instead of 1). At runtime,
the @code{label} form creates a label and returns it to the binding
for @code{alice-label}. When Alice wants to protect a value, she
creates a facet, annotated with her label and two branches. The
positive (left) branch represents the value as it should appear to
her, and the negative (right) to everyone else:

@codeblock|{
            (define alice-board (fac alice-label
                                     (add-pieces (makeboard) x1 y1 ... xn yn)
                                     (makeboard)))
}|

@section[#:tag "semantics"]{Semantics}

@section[#:tag "implementation"]{Faceted Execution as Macros}

@section[#:tag "evaluation"]{Evaluation}

@subsection[#:tag "microbenchmarks"]{Microbenchmarks}

@subsection[#:tag "case-study"]{Case Study: Battleship in Racets}

@section[#:tag "related"]{Related Work}

@section[#:tag "conclusion"]{Conclusion and Future Directions}
