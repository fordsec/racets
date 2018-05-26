#lang racket

(require "facets.rkt")

(provide (except-out
          (all-from-out racket)
          lambda
          #%app)
         facet)

(provide (rename-out [fac-lambda lambda]
                     [fac-app #%app]))

(define-syntax-rule (fac prin v1 v2)
  (facet prin v1 v2))

; Do nothing right now...
(define-syntax-rule (fac-lambda (x ...) expr)
  (lambda (x ...) expr))

; Applications are rewritten to:
; - 
(define-syntax (fac-app stx)
  (syntax-case stx ()
    [(#%app f a0 ...)
     #`(if (facet? f)
           (facet (facet-label f)
                  ((facet-left f) a0 ...)
                  ((facet-right f) a0 ...))
           (f a0 ...))]))
