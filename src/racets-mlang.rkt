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
  #%app)
 ; Rename out our core forms
 (rename-out
  [fac-module-begin #%module-begin]
  [fac-app #%app]
;  [fac-lambda #%plain-lambda]
;  [fac-lambda lambda]
  [fac-if if]
  [fac-continuation-mark with-continuation-mark])

 ; 
 ; Extra macros unique to racets
 ; 

 ; Associate a label with a policy
 let-label
 
 ; Create a facet
 fac
 
 ; Observe a faceted value
 obs

 ; Create a reference cell
 ref

 ; dereference a reference cell
 deref

 ; mutate a reference cell
 ref-set!)

; The starting pc in the program
(define current-pc (make-parameter (set)))

; Lambdas are rewritten into tagged closures so we can implement
; `racets` closures from primitives.
; (define-syntax-rule (fac-lambda xs expr)
;  (fclo (lambda xs expr)))

(define-syntax-rule (let-label l (lambda xs e) body)
  (let ([l (labelpair (gensym 'lab)
                      (lambda xs e))])
    body))

; Construct a faceted value with a specific name and from values of
; pos and neg branches
(define (mkfacet name v1 v2)
  (construct-facet-optimized
   (set->list (set-add (current-pc) (pos name)))
   v1
   v2))

; Syntax for facet construction
(define-syntax-rule (fac l v1 v2)
  (mkfacet (labelpair-name l) v1 v2))

; Observe l e1 e2
; l - The label being checked
; e1 - The argument being passed to the policy 
(define-syntax-rule (obs l e1 e2)
  (let obsf ([lp l]
             [v1 e1] ;; TODO: should e1 be reevaluated?
             [v2 e2])
      (if (facet? v2)
          (let ([v2-name (facet-labelname v2)])
            (if (equal? v2-name (labelpair-name lp))
              (if ((labelpair-pol lp) v1)
                  (facet-left v2)
                  (facet-right v2))
              (let* ([v2-l (facet-left v2)]
                     [v2-r (facet-right v2)])
                (facet v2-name
                       (obsf lp v1 v2-l)
                       (obsf lp v1 v2-r)))))
          v2)))

(define-syntax (fac-module-begin stx)
  (syntax-case stx ()
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

; If
(define-syntax (fac-if stx)
  (syntax-case stx ()
    [(_ guard et ef)
     #`(let iff ([gv guard])
         (if (facet? gv)
             (cond
               [(set-member? (current-pc) (pos (facet-labelname gv)))
                (iff (facet-left gv))]
               [(set-member? (current-pc) (neg (facet-labelname gv)))
                (iff (facet-right gv))]
               [else
                (let*
                    ([left
                      (parameterize ([current-pc (set-add (current-pc)
                                                          (pos
                                                           (facet-labelname gv)))])
                        (iff (facet-left gv)))]
                     [right
                      (parameterize ([current-pc (set-add (current-pc)
                                                          (neg
                                                           (facet-labelname gv)))])
                        (iff (facet-right gv)))])
                  (mkfacet (facet-labelname gv) left right))])
             (if gv et ef)))]))

; ref
(define-syntax (ref stx)
  (syntax-case stx ()
    [(_ vr)
     #`(let ([var vr]) ; let-bind vr to evaluate
         (if (facet? var)
             ; if var is a facet,
             (box (construct-facet-optimized (set->list (current-pc)) var 0))
             ; else
             (box var)))]))

; ref-set!
(define-syntax (ref-set! stx)
  (syntax-case stx ()
    [(_ var e)
     #`(let ([value e])
         (let setf ([var var]
                    [pc (current-pc)])
           (if (box? var)
               ; if var is a facet value
               (set-box! var (construct-facet-optimized (set->list (current-pc)) value (unbox var)))
               ; else
               (mkfacet
                (facet-labelname var)
                (setf
                 (facet-left var)
                 (set-add pc (pos (facet-labelname var))))
                (setf
                 (facet-right var)
                 (set-add pc (neg (facet-labelname var))))))))]))

; deref
(define-syntax (deref stx)
  (syntax-case stx ()
    [(_ vr)
     #`(let dereff ([var (unbox vr)])
         (if (facet? var)
             ; if var is a facet value
             (cond
               [(set-member? (current-pc) (pos (facet-labelname var)))
                (dereff (facet-left var))]
               [(set-member? (current-pc) (neg (facet-labelname var)))
                (dereff (facet-right var))]
               [else
                (mkfacet (facet-labelname var) (dereff (facet-left var)) (dereff (facet-right var)))])
             ; else
             var))]))

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
                                            (pos (facet-labelname func)))])
                     (#%app (facet-left func) a0 ...))]
                  [right
                   (parameterize ([current-pc
                                   (set-add (current-pc)
                                            (neg (facet-labelname func)))])
                     (#%app (facet-right func) a0 ...))])
             (mkfacet (facet-labelname func)
                      left
                      right))
           (func a0 ...)))]))

; Not sure what to do with continuations, we will have to handle other
; continuation-based stuff, too, eventually.
(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
