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

(define-syntax-parameter pc (syntax-rules ()))

; Do nothing right now...
(define-syntax-rule (fac-lambda (x ...) expr)
  (lambda (x ...) expr))
; (fclo (lambda (x ...) expr) #,pc))

(define-syntax (fac-module-begin stx)
  (syntax-case stx ()
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          (define pc (make-parameter (set)))
          body ...)]))

(define-syntax (fac-app stx)
  (syntax-case stx ()
    [(_ f a0 ...)
     #`(if (facet? f)
           (let* ([left
                   (parameterize ([pc (set-add pc (pos (facet-label f)))])
                     ((facet-left f) a0 ...))]
                  [right 
                   (parameterize ([pc (set-add pc (neg (facet-label f)))])
                     ((facet-left f) a0 ...))])
                  (facet (facet-label f)
                         left
                         right))
           (f a0 ...))]))

(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
