; Main macros implementing racets
#lang racket

(require "facets.rkt" (for-syntax "facets.rkt"))
(require (for-syntax racket/syntax))
(require (for-syntax racket/stxparam))
(require (for-syntax racket/set))
(require (for-syntax racket/base syntax/parse))
(require racket/stxparam)

(provide 
 ; Buit on Racket
 (except-out
  (all-from-out racket)
  with-continuation-mark
  #%module-begin
  #%plain-lambda
  lambda
  if
  and
  or
  #%app)
 ; Rename out our core forms
 (rename-out
  [fac-module-begin #%module-begin]
  [fac-app #%app]
  [fac-lambda #%plain-lambda]
  [fac-lambda lambda]
  [fac-if if]
  [fac-continuation-mark with-continuation-mark]
  [fac-and and]
  [fac-or or]
  )

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
 ref-set!

 ; exportable lambda
 ext-lambda

 ; Lazy failure
 ★
 )

; Lambdas are rewritten into tagged closures so we can implement
; `racets` closures from primitives.

(define-syntax (fac-lambda stx)
  (syntax-parse stx
    [(_ xs expr)
    #`(fclo (lambda xs expr))]))

; Create an "external" lambda to hand across a module boundary. This
; is *not* an fclo, but rather it is a "plain" Racket lambda. This is
; potentially bad: if you hand off things to modules that do not
; expect facets, they will crash. So to use this correctly, you must
; use an explicit `obs` form before closing over faceted things in an
; ext-lambda form.

(define-syntax (ext-lambda stx)
  (syntax-parse stx
    [(_ xs expr)
     #`(lambda xs expr)]))

(define-syntax (let-label stx)
  (syntax-parse stx
    [(_ l (lambda xs e) body)
     #`(let ([l (labelpair (gensym 'lab)
                           (lambda xs e))])
         body)]))

(define-syntax (★)
  (syntax-parse
      #`(lfail)))


; Syntax for facet construction
; TODO: FIX!

(define-syntax (fac stx)
  (syntax-parse stx
    [(_ l v1 v2)
     #`(construct-facet-optimized (set->list (set-union (current-pc) (set (pos (labelpair-name l))))) v1 v2) ]))

; Observe l e1 e2
; l - The label being checked
; e1 - The argument being passed to the policy 
(define-syntax (obs stx)
  (syntax-parse stx
    [(_ l e1 e2)
     #`(let obsf ([lp l]
                  [v1 e1]
                  [v2 e2])
         (if (facet? v2)
             (let ([v2-name (facet-labelname v2)])
               (if (equal? v2-name (labelpair-name lp))
                   (if (fac-app (labelpair-pol lp) v1)
                       (facet-left v2)
                       (facet-right v2))
                   (let* ([v2-l (facet-left v2)]
                          [v2-r (facet-right v2)])
                     (mkfacet v2-name
                              (obsf lp v1 v2-l)
                              (obsf lp v1 v2-r)))))
             v2))]))

(define-syntax (fac-module-begin stx)
  (syntax-parse stx
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

; If
(define-syntax (fac-if stx)
  (syntax-parse stx
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
                  (construct-facet-optimized (list (pos (facet-labelname gv))) left right))])
             (if gv et ef)))]))

; ref
(define-syntax (ref stx)
  (syntax-parse stx
    [(_ vr)
     #`(let ([var vr]) ; let-bind vr to evaluate
         (if (facet? var)
             ; if var is a facet,
             (box (construct-facet-optimized (set->list (current-pc)) var (lfail)))
             ; else
             (box var)))]))


; ref-set!
(define-syntax (ref-set! stx)
  (syntax-parse stx
    [(_ var e)
     #`(let ([value e])
         (let setf ([var var]
                    [pc (current-pc)])
           (if (box? var)
               ; if var is a facet value
               (set-box! var (construct-facet-optimized (set->list (current-pc)) value (unbox var)))
               ; else
               (mkfacet
                (facet-labelname (unbox var))
                (setf
                 (facet-left (unbox var))
                 (set-add pc (pos (facet-labelname var))))
                (setf
                 (facet-right (unbox var))
                 (set-add pc (neg (facet-labelname var))))))))]))

; deref
(define-syntax (deref stx)
  (syntax-parse stx
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
(define-syntax (fac-app stx)
  (syntax-parse stx
    [(_ f . args)
     #`(let applyf ([func f])
         (cond
           [(facet? func)
            (let* ([left
                    (parameterize ([current-pc
                                    (set-add (current-pc)
                                             (pos (facet-labelname func)))])
                      (applyf (facet-left func)))]
                   [right
                    (parameterize ([current-pc
                                    (set-add (current-pc)
                                             (neg (facet-labelname func)))])
                      (applyf (facet-right func)))])
              (construct-facet-optimized
               (list (pos (facet-labelname func)))
               left
               right))]
           ; An fclo coming from Racets
           [(fclo? func) ((fclo-clo func) . args)]
           ; Not an fclo. Must be a builtin, etc..
           [else ((facet-fmap* func) . args)]))]))

;
; And/or
;
(define-syntax (fac-and stx)
  (syntax-parse stx
    [(_) #`#t]
    [(_ e0 es ...)
     #`(fac-if e0 (fac-and es ...) #f)]))

(define-syntax (fac-or stx)
  (syntax-parse stx
    [(_) #`#f]
    [(_ e0 es ...)
     #`(fac-if e0 #t (fac-or es ...))]))

; Not sure what to do with continuations, we will have to handle other
; continuation-based stuff, too, eventually.
(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
