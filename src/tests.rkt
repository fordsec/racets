#lang reader "racets.rkt"
(require racket/stxparam)

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  
  ; A few test contracts
  (define con0 (let-label con0 (lambda (x) (x)) con0))
  (define con1 (let-label con1 (lambda (x) x) con1))
  (define con2 (let-label con2 (lambda (x) (not x)) con2))

  ;
  ; Tests for basic function application
  ;

  ; Should be (fac con1 3 1)
  (check-equal?
   ((fac con1 (lambda (x) x) (lambda (x) 1)) 3)
   (fac con1 3 1) "It should return (fac con1 3 1)")

  ; Should be (fac con1 6 9)
  (check-equal?
   ((fac con1 (lambda (x) (+ x x)) (lambda (x) (* x x))) 3)
   (fac con1 6 9) "It should return (fac con1 6 9)")

  ; Should be (fac con2 5 6)
  (check-equal?
   ((fac con2 (lambda (x y) (+ x y)) (lambda (x y) (* x y))) 3 2)
   (fac con2 5 6) "It should return (fac con2 5 6)")

  ; Should be (fac con2 4 4)
  (check-equal?
   ((fac con2 (lambda (x) ((lambda (y) (+ x y)) 2)) (lambda (x) (* x x))) 2)
   (fac con2 4 4) "It should return (fac con2 4 4)")
  
  ;
  ; Tests for set!
  ;
  
  ; Should be (fac con1 2 1)
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #f #t) (set! x 1) (set! x 2)) x))
   (fac con1 2 1) "It should return (fac con1 2 1)")

  ; Should be (fac con1 1 2)
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #t #f) (set! x 1) (set! x 2)) x))
   (fac con1 1 2) "It should return (fac con1 1 2)")

  ; Should be (fac con1 1 1)
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #t #t) (set! x 1) (set! x 2)) x))
   (fac con1 1 1) "It should return (fac con1 1 1)")

  ; Should be (fac con1 2 2)
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #f #f) (set! x 1) (set! x 2)) x))
   (fac con1 2 2) "It should return (fac con1 2 2)")

  ;
  ; !!!This test case is weird!!!
  ; Run it several times, and we get different returned values.
  ;
  #;
  (check-equal?
   (let* ([x (ref #t)])
     (begin (if (fac con1 (fac con2 #t #f) (fac con2 #t #f))
                (ref-set! x #f)
                (ref-set! x 1))
            (deref x)))
   (fac con1 (fac con2 #f 1) (fac con2 #f 1))) ; this is correct I think :)
  
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
   (obs con1 #f (+ 1 2)) 3)
  
  (check-equal? 
   (obs con1 #t (fac con1 1 2))
   1)

  (check-equal? 
   (obs con1 #f (fac con1 1 2))
   2)

  ; Should it be (fac con2 1 2) instead of (fac con1 1 2)?
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

  ; (display (obs con1 #t (fac con1 (fac con2 1 2) (fac con2 3 4))))
  
  (check-equal?
   (obs con2 #t (obs con1 #t (fac con1 (fac con2 1 2) (fac con2 3 4))))
   2)

  (check-equal?
   (obs con1 #t (fac con0 (fac con1 (fac con2 1 2) (fac con2 3 4)) (fac con1 (fac con2 5 6) (fac con2 7 8))))
   (fac con0 (fac con2 1 2) (fac con2 5 6)) "Should be (fac con0 (fac con2 1 2) (fac con2 5 6))")

  ;
  ; Some combined test cases for set!, app, if, obs
  ;
  
  ; Test case that combines obs and app.
  (check-equal?
   (obs con2 #t ((fac con2 (lambda (x) ((lambda (y) (+ x y)) 2)) (lambda (x) (* x x))) 2))
   4 "It should return 4")

  ; Test case that combines app, set! and if.
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #f #t) (set! x (lambda (y) (+ y y))) (set! x (lambda (y) (* y y)))) (x 3)))
   (fac con1 9 6) "It should return (fac con1 9 6)")

  ; Test case that combines obs, set!, and if.
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #f #t) (set! x 1) (set! x 2)) (obs con1 #t x)))
   2 "It should return 2")

  ; Test cases that combine obs, set!, app and if.
  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #f #t) (set! x (lambda (y) (+ y y))) (set! x (lambda (y) (* y y)))) ((obs con1 #t x) 3)))
   9 "It should return 9")

  (check-equal?
   (let ([x 0])
     (begin (if (fac con1 #t #f) (set! x (lambda (y) (+ y y))) (set! x (lambda (y) (* y y)))) ((obs con1 #t x) 3)))
   6 "It should return 6")

  ; A complicated test case that involves app, set, obs, and if
  #;
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #t)
                (ref-set! x (fac con2 (lambda (y) (+ y y)) (lambda (y) (* y y y))))
                (ref-set! x (fac con2 (lambda (y) (- y y y)) (lambda (y) (* y y)))))
            ((obs con1 #t (obs con1 #f (deref x))) 2)))
   (fac con2 -4 4) "It should return (fac con2 -4 4)"))
