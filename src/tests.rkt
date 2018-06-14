#lang reader "racets.rkt"
(require racket/stxparam)

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  
  ; A few test contracts
  (define con1 (let-label con1 (lambda (x) x) con1))
  (define con2 (let-label con1 (lambda (x) (not x)) con1))

  ;
  ; Tests for basic function application
  ;

  ; Should be (fac con1 3 1)
  (check-equal?
   ((fac con1 (lambda (x) x) (lambda (x) 1)) 3)
   (fac con1 3 1))
  
  ;
  ; Tests for set!
  ;
  
  ; Should be (fac con1 2 1)
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #f #t) (set! x 1) (set! x 2)) x))
   (fac con1 2 1))

  ; 
  ; Tests for builtins
  ; 
  (check-equal?
   ((fac con1 + -) 3)
   (fac con1 3 -3))
  
  ;
  ; Tests for obs
  ;
  (check-equal?
   (obs con1 #t (+ 1 2)) 3)

  (check-equal? 
   (obs con1 #t (fac con1 1 2))
   1)

  (check-equal? 
   (obs con1 #f (fac con1 1 2))
   2)
  
  (check-equal?
   (obs con2 #t (fac con1 1 2))
   (fac con1 1 2))

  ; If you have:
  ;     (fac con1 (fac con2 1 2) (fac con2 3 4))
  ; 
  ; Observe con2 will "dig into" each branch of the facet, and then
  ; call observe on each branch. Then, it will reconstruct a new facet
  ; where each branch is wrapped in con1
  (check-equal?
   (obs con2 #t (fac con1 (fac con2 1 2) (fac con2 3 4)))
   (fac con1 2 4))

  (check-equal?
   (obs con2 #t (obs con1 #t (fac con1 (fac con2 1 2) (fac con2 3 4))))
   2))

  ;(check-equal?
  ; ((fac 'bob + -) 3);
  ;(fac 'bob 3 -3))
