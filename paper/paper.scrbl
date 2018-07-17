#lang scribble/acmart @acmlarge @review @natbib
@(require scriblib/bibtex
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

@section[#:tag "semantics"]{Semantics}

@section[#:tag "implementation"]{Faceted Execution as Macros}

@section[#:tag "evaluation"]{Evaluation}

@subsection[#:tag "microbenchmarks"]{Microbenchmarks}

@subsection[#:tag "case-study"]{Case Study: Battleship in Racets}

@section[#:tag "related"]{Related Work}

@section[#:tag "conclusion"]{Conclusion and Future Directions}
