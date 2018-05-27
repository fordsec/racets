#lang racket

(require "facets.rkt" (for-syntax "facets.rkt"))
(require (for-syntax racket/syntax))
(require (for-syntax racket/stxparam))
(require (for-syntax racket/set))
(require racket/stxparam)

(provide (except-out
          (all-from-out racket)
          lambda
          with-continuation-mark
          #%module-begin
          #%app)
         fac)

(provide (rename-out [fac-lambda lambda]
                     [fac-module-begin #%module-begin]
                     [fac-app #%app]
                     [fac-continuation-mark with-continuation-mark]))

(define-syntax-rule (fac prin v1 v2)
  (facet prin v1 v2))

; Do nothing right now...
(define-syntax-rule (fac-lambda (x ...) expr)
  (lambda (x ...) expr))

(define-syntax (fac-module-begin stx)
  (syntax-case stx ()
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          (define-syntax-parameter pc (set))
          body ...)]))

(define-syntax-parameter pc 
 (lambda (stx)
      (raise-syntax-error (syntax-e stx) "can only be used inside aif")))

(define-syntax (fac-app stx)
  (syntax-case stx ()
    [(#%app f a0 ...)
     #`(if (facet? f)
           (let* ([left
                   (syntax-parameterize ([pc (set-add #'pc (pos (facet-label f)))])
                     ((facet-left f) a0 ...))]
                  [right 
                   (syntax-parameterize ([pc (set-add #'pc (neg (facet-label f)))])
                     ((facet-left f) a0 ...))])
                  (#'facet (facet-label f)
                   left-side
                   right-side))
           (f a0 ...))]))

(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
