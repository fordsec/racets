#lang racket

(provide (all-defined-out))

(define label? symbol?)

; Literals
(struct pos (lab) #:transparent)
(struct neg (lab) #:transparent)

; Facets
(struct facet (labelname left right) #:transparent)

; Labels are a name plus a lambda (policy) that implements them
(struct labelpair (name pol) #:transparent)

; Tagged faceted closures
(struct fclo (clo) #:transparent)

; Lazy failure
(struct lfail ())

; Label comparison
(define (label<? l1 l2)
  (match (cons l1 l2)
    [(cons '∞ _) #f]
    [(cons '∞ '∞) #f]
    [(cons _ '∞) #t]
    [else (symbol<? l1 l2)]))

; Optimized facet construction ala Austin. This follows (nearly
; exactly) the formulation in Austin et al.'s optimized facet
; construction: Figure 6 in their paper.
(define (construct-facet-optimized pc v default-value)
  (define (head v)
    (match v
      [(cons (pos k) _) k]
      [(cons (neg k) _) k]
      [(facet k _ _) k]
      [else '∞]))
  (match (list pc v default-value)
    [(list '() _ _) v]
    [(list (cons (pos k) rst) (facet k va vb) (facet k vc vd)) 
     (facet k (construct-facet-optimized rst va vc) vd)]
    [(list (cons (neg k) rst) (facet k va vb) (facet k vc vd))
     (facet k vc (construct-facet-optimized rst vb vd))]
    [(list pc (facet k va vb) (facet k vc vd))
     #:when (label<? k (head pc))
     (facet k (construct-facet-optimized pc va vc)
            (construct-facet-optimized pc vb vd))]
    [(list (cons (pos k) rst) (facet k va vb) vo)
     #:when (label<? k (head vo))
     (facet k (construct-facet-optimized rst va vo) vo)]
    [(list (cons (neg k) rst) (facet k va vb) vo)
     #:when (label<? k (head vo))
     (facet k vo (construct-facet-optimized rst vb vo))]
    [(list (cons (pos k) rst) vn (facet k va vb))
     #:when (label<? k (head vn))
     (facet k (construct-facet-optimized rst vn va) vb)]
    [(list (cons (neg k) rst) vn (facet k va vb))
     #:when (label<? k (head vn))
     (facet k va (construct-facet-optimized rst vn vb))]
    [(list (cons (pos k) rst) vn vo)
     #:when (and (label<? k (head vn)) (label<? k (head vo)))
     (facet k (construct-facet-optimized rst vn vo) vo)]
    [(list (cons (neg k) rst) vn vo)
     #:when (and (label<? k (head vn)) (label<? k (head vo)))
     (facet k vo (construct-facet-optimized rst vn vo))]
    [(list pc (facet k va vb) vo)
     #:when (and (label<? k (head vo)) (label<? k (head pc)))
     (facet k (construct-facet-optimized pc va vo)
            (construct-facet-optimized pc vb vo))]
    [(list pc vn (facet k va vb))
     #:when (and (label<? k (head vn)) (label<? k (head pc)))
     (facet k (construct-facet-optimized pc vn va)
            (construct-facet-optimized pc vn vb))]))


; The starting pc in the program
(define current-pc (make-parameter (set)))

; Construct a faceted value with a specific name and from values of
; pos and neg branches
(define (mkfacet name v1 v2)
  (construct-facet-optimized
   (set->list (set-add (current-pc) (pos name)))
   v1
   v2))

;; Requires all faceted values to be in order (according to label<?)
(define ((facet-fmap* f) . fvs)
  (if (ormap (match-lambda
               [(facet _ _ _) #t]
               [else #f]) fvs)
      (let* ([l (foldl
                 (lambda (fv ll)
                   (match fv
                     [(facet lab lv rv) (if (label<? ll lab) ll lab)]
                     [else ll]))
                 '∞
                 fvs)]
             [fvs+ (map
                    (match-lambda
                      [(and v (facet lab lv rv)) #:when (eq? lab l) v]
                      [v (facet l v v)])
                    fvs)]
             [lvs (map (match-lambda [(facet _ lv _) lv]) fvs+)]
             [rvs (map (match-lambda [(facet _ _ rv) rv]) fvs+)]
             [lv (apply (facet-fmap* f) lvs)]
             [rv (apply (facet-fmap* f) rvs)])
        (construct-facet-optimized (list (pos l)) lv rv))
      (apply f fvs)))

(define (prim-to-δ op)
  (match op
    ['+ +]
    ['* *]
    ['- -]
    ['=
     (λ xs (let ([ret 1]
                 [fst (first xs)])
             (map (lambda (v)
                    (if (not (equal? v fst))
                        (set! ret 0)
                        (void))) xs)
             ret))]))
