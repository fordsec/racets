#lang reader "racets.rkt"

(define con0 (let-label con0 (lambda (x) (x)) con0))
(define con1 (let-label con1 (lambda (x) x) con1))

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

  ; a seem-to-be-trivial test case that involves multiple levels of if-else conditional clause to test its robustness
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

  ; Test cases for built-in +
  (check-equal?
   (+ (fac con0 2 0) (fac con1 1 0))
   (fac con0 (fac con1 3 2) (fac con1 1 0))
   "It should return (fac con0 (fac con1 3 2) (fac con1 1 0))")

  (check-equal?
   (+ (fac con0 2 1))
   (fac con0 2 1))
  
  (check-equal?
   (+ (fac con1 (fac con2 0 1) (fac con2 2 3)) (fac con0 (fac con2 4 5) (fac con2 6 7)))
   (fac con1
        (fac con0
             (fac con2 4 6)
             (fac con2 6 8))
        (fac con0
             (fac con2 6 8)
             (fac con2 8 10))))

  (check-equal?
   (+ (fac con1 0 1) 1)
   (fac con1 1 2))
  
  (check-equal?
   (+ (fac con0 1 0) (+ (fac con1 2 3) (+ (fac con2 4 5) (fac con3 6 7))))
   (fac con0
        (fac con1
             (fac con2
                  (fac con3 13 14)
                  (fac con3 14 15))
             (fac con2
                  (fac con3 14 15)
                  (fac con3 15 16)))
        (fac con1
             (fac con2
                  (fac con3 12 13)
                  (fac con3 13 14))
             (fac con2
                  (fac con3 13 14)
                  (fac con3 14 15)))))

  ; Test cases for built-in -
  (check-equal?
   (- (fac con0 1 2))
   (fac con0 -1 -2))

  (check-equal?
   (- (fac con0 4 5) (fac con1 3 2))
   (fac con0 (fac con1 1 2) (fac con1 2 3)))

  (check-equal?
   (- (fac con0 (fac con1 10 9) (fac con1 8 7)) (fac con0 (fac con1 1 2) (fac con1 3 4)))
   (fac con0 (fac con1 9 7) (fac con1 5 3)))

  (check-equal?
   (- (fac con0 (fac con1 9 8) (fac con2 7 6)))
   (fac con0 (fac con1 -9 -8) (fac con2 -7 -6)))

  ; !!!Is this test case correct? Need review!!!
  (check-equal?
   (- (fac con0 (fac con1 9 8) (fac con2 7 6)) (fac con1 (fac con0 1 2) (fac con2 3 4)))
   (fac con0 (fac con1 8 (fac con2 5 4)) (fac con1 (fac con2 5 4) (fac con2 4 2))))

  (check-equal?
   (- (fac con0 10 9) 2)
   (fac con0 8 7))

  (check-equal?
   (- (fac con0 10 9) (- (fac con1 8 7) (- (fac con2 6 5) (fac con3 4 3))))
   (fac con0
        (fac con1
             (fac con2
                  (fac con3 4 5)
                  (fac con3 3 4))
             (fac con2
                  (fac con3 5 6)
                  (fac con3 4 5)))
        (fac con1
             (fac con2
                  (fac con3 3 4)
                  (fac con3 2 3))
             (fac con2
                  (fac con3 4 5)
                  (fac con3 3 4)))))

  ; Test cases for built-in *
  (check-equal?
   (* (fac con0 4 5) 2)
   (fac con0 8 10))
  
  (check-equal?
   (* (fac con0 2 3) (fac con0 4 5))
   (fac con0 8 15))

  (check-equal?
   (* (fac con0 4 5) (fac con1 6 7))
   (fac con0 (fac con1 24 28) (fac con1 30 35)))

  ; !!!Need to verify that this test case is correct!!!
  (check-equal?
   (* (fac con1 (fac con2 2 3) (fac con3 4 5)) (fac con0 (fac con1 6 7) (fac con2 8 9)))
   (fac con0
        (fac con1
             (fac con2 12 18)
             (fac con3 28 35))
        (fac con1
             (fac con2 16 27)
             (fac con2 (fac con3 32 40) (fac con3 36 45)))))

  (check-equal?
   (* (fac con0 (fac con1 3 4) (fac con2 5 6)) (fac con3 (fac con1 10 11) (fac con2 7 8)))
   (fac con0
        (fac con3
             (fac con1 30 44)
             (fac con1
                  (fac con2 21 24)
                  (fac con2 28 32)))
        (fac con3
             (fac con2
                  (fac con1 50 55)
                  (fac con1 60 66))
             (fac con2 35 48))))

  ; a complicated test case that involves *
  (check-equal?
   (* (fac con0 (fac con1 1 2) (fac con2 3 4))
      (* (fac con1 (fac con2 5 6) (fac con0 7 8))
         (* (fac con2 9 10) (fac con3 11 12))))
   (fac con0
        (fac con1
             (fac con2
                  (fac con3 495 540)
                  (fac con3 660 720))
             (fac con2
                  (fac con3 1386 1512)
                  (fac con3 1540 1680)))
        (fac con1
             (fac con2
                  (fac con3 1485 1620)
                  (fac con3 2640 2880))
             (fac con2
                  (fac con3 2376 2592)
                  (fac con3 3520 3840)))))
  
  ; Test cases for built-in /
  (check-equal?
   (/ (fac con0 10 6) 2)
   (fac con0 5 3))

  (check-equal?
   (/ (fac con1 8 10) (fac con1 4 5))
   (fac con1 2 2))

  (check-equal?
   (/ (fac con1 20 40) (fac con2 4 5))
   (fac con1 (fac con2 5 4) (fac con2 10 8)))

  (check-equal?
   (/ (fac con1 (fac con2 100 120) (fac con3 120 60)) (fac con0 (fac con2 2 5) (fac con3 3 4)))
   (fac con1
        (fac con0
             (fac con2 50 24)
             (fac con2
                  (fac con3 (/ 100 3) 25)
                  (fac con3 40 30)))
        (fac con0
             (fac con3
                  (fac con2 60 24)
                  (fac con2 30 12))
             (fac con3 40 15))))
  
  ; Test cases for the built-in and
  ; it fails for now.
  #;
  (check-equal?
   (and (fac con0 #t (void)) (fac con1 #f (void)))
   (fac con0 (fac con1 #f (void)) (void)))

  (check-equal?
   (and (fac con0 #t #f) (fac con1 #f #t))
   (fac con0 (fac con1 #f #t) (fac con1 #f #f)))

  (check-equal?
   (and (fac con2 #t #f))
   (fac con2 #t #f))

  (check-equal?
   (and (fac con1 #t #f) #f)
   (fac con1 #f #f))

  (check-equal?
   (and (fac con0 #f #t) (fac con1 #t #f) (fac con2 #t #f))
   (fac con0
        (fac con1 (fac con2 #f #f) (fac con2 #f #f))
        (fac con1 (fac con2 #t #f) (fac con2 #f #f))))

  (check-equal?
   (and #f (fac con0 #t #f))
   #f)
  
  (check-equal?
   (and)
   #t)

  ; Test case for the built-in or
  #;(check-equal?
   (or (fac con2 (fac con1 1 2) (fac con1 #t #f)))
   (fac con2 (fac con1 1 2) (fac con1 #t #f)))

  (check-equal?
   (or (fac con2 #t #f) (fac con1 #t #f))
   (fac con2 (fac con1 #t #t) (fac con1 #t #f)))

  (check-equal?
   (or (fac con1 #f #f) #t)
   (fac con1 #t #t))

  (check-equal?
   (or (fac con2 #t #f) (fac con1 #t #f) (fac con3 #t #f))
   (or (fac con2 #t #f) (or (fac con1 #t #f) (fac con3 #t #f))))
  
  (check-equal?
   (or)
   #f)
  
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

  ; Should have a complicated test case that involves built-in, ref-set!, obs, and if
  (check-equal?
   (let ([x (ref 0)])
     (begin (if (fac con1 #t #f)
                (ref-set! x (+ (fac con0 2 0) (fac con1 1 0)))
                (ref-set! x (- (fac con0 4 5) (fac con1 3 2)))))
     (deref x))
   (fac con0 (fac con1 3 2) (fac con1 1 3)))
  
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
    (fac con3 1 3)))
  
  ; A test of faceted builtins on faceted values
  (check-equal?
   ((fac con1 (fac con2 + -) (fac con2 * list))
    (begin (inc)
           (fac con1
                (fac con2 (begin (inc) 1) (begin (inc) 2))
                (fac con3 (begin (inc) 3) (begin (inc) 4))))
    (fac con3 (begin (inc) 2) (begin (inc) 1)))
   (fac con1
        (fac con2
             (fac con1 (fac con3 3 2) 3)
             (fac con1
                  (fac con2
                       (fac con1 (fac con3 -1 0) (fac con3 0 1))
                       (fac con3 0 1))
                  3))
        (fac con2
             (fac con1 (fac con3 2 1) (fac con3 6 4))
             (fac con1
                  (fac con2
                       (fac con3 '(1 2) '(1 1))
                       (fac con3 '(2 2) '(2 1)))
                  (fac con3 '(3 2) '(4 1))))))


  #;
  (check-equal?
   ((fac con1 (fac con2 + -) (fac con2 * list))
    (begin (inc)
           (fac con1
                (fac con2 (begin (inc) 1) (begin (inc) 2))
                (fac con3 (begin (inc) 3) (begin (inc) 4))))
    (fac con3 (begin (inc) 2) (begin (inc) 1)))))
