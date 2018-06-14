; Main macros implementing racets
#lang racket

(require "facets.rkt" (for-syntax "facets.rkt"))
(require (for-syntax racket/syntax))
(require (for-syntax racket/stxparam))
(require (for-syntax racket/set))
(require racket/stxparam)

(provide 
 ; Buit on Racket
 (except-out
  (all-from-out racket)
  with-continuation-mark
  #%module-begin
;  #%plain-lambda
;  lambda
  if
  set!
  #%app)
 ; Rename out our core forms
 (rename-out
  [fac-module-begin #%module-begin]
  [fac-app #%app]
;  [fac-lambda #%plain-lambda]
;  [fac-lambda lambda]
  [fac-if if]
  [fac-set! set!]
  [fac-continuation-mark with-continuation-mark])

 ; 
 ; Extra macros unique to racets
 ; 

 ; Associate a label with a policy
 let-label
 
 ; Create a facet
 fac
 
 ; Observe a faceted value
 obs)

; The starting pc in the program
(define current-pc (make-parameter (set)))

; Lambdas are rewritten into tagged closures so we can implement
; `racets` closures from primitives.
; (define-syntax-rule (fac-lambda xs expr)
;  (fclo (lambda xs expr)))

; Probably not quite what we want...
(define-syntax-rule (let-label l (lambda xs e) body)
  (let ([l (labelpair (gensym 'lab) (lambda xs e))]) body))

; Construct a faceted value with a specific name and from values of
; pos and neg branches
(define (mkfacet name v1 v2)
  (construct-facet-optimized
   (set->list (set-add (current-pc) (pos name))) v1 v2))

; Syntax for facet construction
(define-syntax-rule (fac l v1 v2)
  (mkfacet l v1 v2))

; Observe l e1 e2
; l - The label being checked
; e1 - The argument being passed to the policy 
(define-syntax-rule (obs l e1 e2)
  (let* ([v1 e1]
         [v2 e2]
         [policy l])
    (if (facet? v2)
        ; equal?
        (if (equal? (facet-label v2) policy)
            ; Evaluate the contract
            (if ((labelpair-pol policy) v1)
                ; If true, return left side of facet
                (facet-left v2)
                ; If false, return right side of facet
                (facet-right v2))
            ; Otherwise, evaluate left and right side, then make a
            ; facet of them both.
            ; Zhanpeng: try to implement this yourself over the next
            ; day or two
            (void))
        v2)))

(define-syntax (fac-module-begin stx)
  (syntax-case stx ()
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

; set!
(define-syntax-rule (fac-set! id expr)
  (set! id
        (construct-facet-optimized (set->list (current-pc)) expr id)))

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

; Faceted application
; Broken for builtins.
(define-syntax (fac-app stx)
  (syntax-case stx ()
    [(_ f a0 ...)
     #`(let ([func f])
         (if (facet? func)
           (let* ([left
                   (parameterize ([current-pc
                                   (set-add (current-pc)
                                            (pos (facet-label func)))])
                     (#%app (facet-left func) a0 ...))]
                  [right
                   (parameterize ([current-pc
                                   (set-add (current-pc)
                                            (neg (facet-label func)))])
                     (#%app (facet-right func) a0 ...))])
             (mkfacet (facet-label func)
                      left
                      right))
           (func a0 ...)))]))

; Not sure what to do with continuations, we will have to handle other
; continuation-based stuff, too, eventually.
(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
