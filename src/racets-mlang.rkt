#lang racket

(require "facets.rkt" (for-syntax "facets.rkt"))
(require (for-syntax racket/syntax))
(require (for-syntax racket/stxparam))
(require (for-syntax racket/set))
(require racket/stxparam)

(provide (except-out
          (all-from-out racket)
          with-continuation-mark
          #%module-begin
          #%plain-lambda
          #%variable-reference
          lambda
          if
          set!
          #%app)
         (rename-out
          [fac-module-begin #%module-begin]
          [fac-app #%app]
          [fac-lambda #%plain-lambda]
          [fac-lambda lambda]
          [fac-if if]
          [fac-set! set!]
          [fac-var #%variable-reference]
          [fac-continuation-mark with-continuation-mark])
         let-label
         fac)

(struct labelpair (name con) #:transparent)

; The starting pc in the program
(define current-pc (make-parameter (set)))

(define-syntax-rule (fac-lambda xs expr)
  (fclo (lambda xs expr)))

; Probably not quite what we want...
(define-syntax-rule (let-label l (lambda xs e) body)
  (let ([l (labelpair (gensym 'lab) (lambda xs e))]) body))

(define (mkfacet name v1 v2)
  (construct-facet-optimized
   (set->list (set-add (current-pc) (pos name))) v1 v2))

; Construct a facet
(define-syntax-rule (fac l v1 v2)
  (mkfacet (labelpair-name l) v1 v2))

(define-syntax-rule (obs l e1 e2)
  (let* ([v1 e1]
         [v2 e2]
         [contract-clo l])
    (if (facet? v2)
        t
        f)))

(define-syntax (fac-module-begin stx)
  (syntax-case stx ()
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

; set!
(define-syntax-rule (fac-set! id expr)
  (set! id
        (construct-facet-optimized (set->list (current-pc)) expr id)))

(define-syntax-rule (fac-var stx)
  (error "bad syntax"))

; If
(define-syntax (fac-if stx)
  (syntax-case stx ()
    [(_ guard et ef)
     #`(let loop ([gv guard])
         (if (facet? gv)
             (cond
               [(set-member? (current-pc) (pos (facet-label gv)))
                (loop (facet-left gv))]
               [(set-member? (current-pc) (neg (facet-label gv)))
                (loop (facet-right gv))]
               [else
                (let*
                    ([left
                      (parameterize ([current-pc (set-add (current-pc)
                                                          (pos
                                                           (facet-label gv)))])
                        (loop (facet-left gv)))]
                     [right
                      (parameterize ([current-pc (set-add (current-pc)
                                                          (neg
                                                           (facet-label gv)))])
                        (loop (facet-right gv)))])
                  (mkfacet (facet-label gv) left right))])
             (if gv et ef)))]))

; Faceted application.
; Very broken for builtins
(define-syntax (fac-app stx)
  (syntax-case stx ()
    [(_ f a0 ...)
     #`(if (facet? f)
           (let* ([left
                   (parameterize ([current-pc
                                   (set-add (current-pc)
                                            (pos (facet-label f)))])
                     (#%app (facet-left f) a0 ...))]
                  [right
                   (parameterize ([current-pc
                                   (set-add (current-pc)
                                            (neg (facet-label f)))])
                     (#%app (facet-right f) a0 ...))])
             (mkfacet (facet-label f)
                      left
                      right))
           ((fclo-clo f) a0 ...))]))

;; Here's a version we're playing around with, but it's broken..
;; (define-syntax (fac-app stx)
;;   (syntax-case stx ()
;;     [(_ f a0 ...)
;;      #`(if (facet? f)
;;            (let* ([real-args (if (fclo? f)
;;                                  (list a0 ...)
;;                                  (error "unimplemented"))]
;;                   [real-clo (if (fclo? f) (fclo-clo f) f)]
;;                   [left
;;                    (parameterize ([current-pc
;;                                    (set-add (current-pc)
;;                                             (pos (facet-label f)))])
;;                      (apply real-clo real-args))]
;;                   [right
;;                    (parameterize ([current-pc
;;                                    (set-add (current-pc)
;;                                             (neg (facet-label f)))])
;;                      (apply real-clo real-args))])
;;              (facet (facet-label f)
;;                     left
;;                     right))
;;            ((fclo-clo f) a0 ...))]))

(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
