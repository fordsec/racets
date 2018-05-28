#lang reader "racets.rkt"
(require racket/stxparam)

; ((fac 'bob (lambda (x) x) (lambda (x) 1)) 3)
; (if (fac 'alice #t #f) 1 0)
; (if (fac 'bob (fac 'alice #t #f) (fac 'alice #f #t)) 1 0)

; Should be (fac bob 2 1)
(let ([x 0])
  (let-label
   bob
   (lambda (x) x)
   (begin (if (fac bob #f #t) (set! x 1) (set! x 2)) x)))

;(let-label
; bob
; (lambda (x) x)
; ((fac bob (lambda (x) x) (lambda (x) 1)) 3))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  ;; (check-equal?
  ;;  (let-label
  ;;   bob
  ;;   (lambda (x) x)
  ;;   ((fac bob (lambda (x) x) (lambda (x) 1)) 3)
  ;;   (fac bob 3 1)))
   )

  ;(check-equal?
  ; ((fac 'bob + -) 3);
  ;(fac 'bob 3 -3))
