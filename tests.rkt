#lang reader "racets.rkt"
(require racket/stxparam)

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define num 0)
  (define (inc) (set! num (+ num 1)))
  
  ; A few test contracts
  (define con0 (let-label con0 (lambda (x) (x)) con0))
  (define con1 (let-label con1 (lambda (x) x) con1))
  (define con2 (let-label con2 (lambda (x) (not x)) con2))
  (define con3 (let-label con3 (lambda (x) (+ x x)) con3))

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
  ; Tests for ref-set!
  ;
  
  ; Should be (fac con1 2 1)
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #t) (ref-set! x 1) (ref-set! x 2)) (deref x)))
   (fac con1 2 1) "It should return (fac con1 2 1)")

  ; Should be (fac con1 1 2)
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #t #f) (ref-set! x 1) (ref-set! x 2)) (deref x)))
   (fac con1 1 2) "It should return (fac con1 1 2)")

  ; Should be (fac con1 1 1)
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #t #t) (ref-set! x 1) (ref-set! x 2)) (deref x)))
   (fac con1 1 1) "It should return (fac con1 1 1)")

  ; Should be (fac con1 2 2)
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #f) (ref-set! x 1) (ref-set! x 2)) (deref x)))
   (fac con1 2 2) "It should return (fac con1 2 2)")

  ; Should be (fac con1 (fac con2 #f 1) (fac con2 #f 1))
  (check-equal?
   (let* ([x (ref #t)])
     (begin (if (fac con1 (fac con2 #t #f) (fac con2 #t #f))
                (ref-set! x #f)
                (ref-set! x 1))
            (deref x)))
   (fac con1 (fac con2 #f 1) (fac con2 #f 1))) ; this is correct I think :)

  ;
  ; Tests for ref, ref-set!, deref
  ;
  (check-equal?
   (deref (ref 2))
   2)

  (check-equal?
   (let ([x (ref 2)])
     (begin (if (fac con1 #t #f)
                (ref-set! x 2)
                (ref-set! x 1))
            (obs con1 #t (deref x))))
   2 "It should return 2.")

  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #t #f)
                (if (fac con2 #t #f)
                    (ref-set! x 1)
                    (ref-set! x 2))
                (ref-set! x 3))
            (obs con1 #t (obs con2 #f (deref x)))))
   1 "It should return 1.")

  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #t #f)
                (if (fac con2 #t #f)
                    (ref-set! x 1)
                    (ref-set! x 2))
                (ref-set! x 3))
            (deref x)))
   (fac con1 (fac con2 1 2) 3) "It should return (fac con1 (fac con2 1 2) 3).")

  ; a seem-to-be-trivial test case that involves multiple levels of if-else conditional clause
  (check-equal?
   (let ([x (ref 123)])
     (begin (if (fac con1 #t #f)
                (if (fac con2 #t #f)
                    (if (fac con0 #t #f)
                        (if (fac con1 #t #f)
                            (if (fac con2 #t #f)
                                (ref-set! x 0)
                                (ref-set! x 1))
                            (ref-set! x 2))
                        (ref-set! x 3))
                    (ref-set! x 4))
                (ref-set! x 5))
            (deref x)))
   (fac con1 (fac con2 (fac con0 (fac con1 (fac con2 0 1) 2) 3) 4) 5)
   "It should return (fac con1 (fac con2 (fac con0 (fac con1 (fac con2 0 1) 2) 3) 4) 5).")

  (check-equal?
   (let ([x (ref 2)])
     (ref-set! x 3)
     (deref x))
   3 "It should return 3.")

  (check-equal?
   (let* ([x (ref 1)]
          [y (ref 0)]
          [z (ref (fac con1 x y))])
     (ref-set! z 2)
     (obs con1 #t (deref z)))
   2 "It should return 2")

  
  
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
  
  (check-equal?
   (obs con2 #t (obs con1 #t (fac con1 (fac con2 1 2) (fac con2 3 4))))
   2)

  (check-equal?
   (obs con1 #t (fac con0 (fac con1 (fac con2 1 2) (fac con2 3 4)) (fac con1 (fac con2 5 6) (fac con2 7 8))))
   (fac con0 (fac con2 1 2) (fac con2 5 6)) "Should be (fac con0 (fac con2 1 2) (fac con2 5 6))")

  ;
  ; Some combined test cases for ref-set!, app, if, obs
  ;
  
  ; Test case that combines obs and app.
  (check-equal?
   (obs con2 #t ((fac con2 (lambda (x) ((lambda (y) (+ x y)) 2)) (lambda (x) (* x x))) 2))
   4 "It should return 4")

  ; Test case that combines app, ref-set! and if.
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #t)
                (ref-set! x (lambda (y) (+ y y)))
                (ref-set! x (lambda (y) (* y y))))
            ((deref x) 3)))
   (fac con1 9 6) "It should return (fac con1 9 6)")

  ; Test case that combines obs, ref-set!, and if.
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #t)
                (ref-set! x 1)
                (ref-set! x 2))
            (obs con1 #t (deref x))))
   2 "It should return 2")

  ; Test cases that combine obs, ref-set!, app and if.
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #t)
                (ref-set! x (lambda (y) (+ y y)))
                (ref-set! x (lambda (y) (* y y))))
            ((obs con1 #t (deref x)) 3)))
   9 "It should return 9")

  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #t #f)
                (ref-set! x (lambda (y) (+ y y)))
                (ref-set! x (lambda (y) (* y y))))
            ((obs con1 #t (deref x)) 3)))
   6 "It should return 6")

  ; A complicated test case that involves app, ref-set!, obs, and if
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #f #t)
                (ref-set! x (fac con2 (lambda (y) (+ y y)) (lambda (y) (* y y y))))
                (ref-set! x (fac con2 (lambda (y) (- y y y)) (lambda (y) (* y y)))))
            ((obs con1 #t (obs con1 #f (deref x))) 2)))
   (fac con2 4 8) "It should return (fac con2 4 8)")

  ; A more ambitious test of applying builtins to faceted values
  (check-equal? 
   (-
    (begin (inc)
           (fac con1
                (fac con2 (begin (inc) 1) (begin (inc) 2))
                (fac con3 (begin (inc) 3) (begin (inc) 4))))
    (fac con3 (begin (inc) 2) (begin (inc) 1)))
   (fac
    con1
    (fac con2 (fac con3 -1 0) (fac con3 0 1))
    (fac con3 1 3))))
