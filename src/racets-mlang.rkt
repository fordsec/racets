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

; The starting pc in the program
(define current-pc (make-parameter (set)))

(define-syntax-rule (fac prin v1 v2)
  (facet prin v1 v2))

; Do nothing right now. Eventually: extend lambda to capture pc in
; closure.
(define-syntax-rule (fac-lambda (x ...) expr)
  (lambda (x ...) expr))

(define-syntax (fac-module-begin stx)
  (syntax-case stx ()
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

(define-syntax (fac-app stx)
  (syntax-case stx ()
    [(_ f a0 ...)
     #`(if (facet? f)
           (let* ([left
                   (parameterize ([current-pc
                                   (set-add (#%app current-pc)
                                            (pos (facet-label f)))])
                     (#%plain-app (facet-left f) a0 ...))]
                  [right
                   (parameterize ([current-pc
                                   (set-add (#%app current-pc)
                                            (neg (facet-label f)))])
                     (#%plain-app (facet-right f) a0 ...))])
             (facet (facet-label f)
                    left
                    right))
           (#%plain-app f a0 ...))]))

(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
