#lang racket

(provide (all-defined-out))

(define label? symbol?)

; Literals
(struct pos (lab) #:transparent)
(struct neg (lab) #:transparent)

; Facets
(struct facet (label left right) #:transparent)

; Labels are a name plus a lambda (policy) that implements them
(struct labelpair (name pol) #:transparent)

; Tagged faceted closures
(struct fclo (clo))

; Label comparison
(define (label<? l1 l2)
  (match (cons l1 l2)
    [(cons '∞ _) #f]
    [(cons '∞ '∞) #f]
    [(cons '∞ _) #t]
    [else
     (if (void? l1)
         l2
         (if (void? l2)
             l1
             (symbol<? l1 l2)))]))

; Optimized facet construction ala Austin. This follows (nearly
; exactly) the formulation in Austin et al.'s optimized facet
; construction: Figure 6 in their paper.
(define (construct-facet-optimized pc v default-value)
  (define (head v)
    (match v
      [(list (pos (labelpair k _)) _) k]
      [(list (neg (labelpair k _)) _) k]
      [(facet (labelpair k _) _ _) k]
      [else '∞]))
  (match (list pc v default-value)
    [(list '() _ _) v]
    [(list (cons (pos (labelpair k p)) rst) (facet (labelpair k p) va vb)
           (facet (labelpair k p) vc vd))
     (facet (labelpair k p) (construct-facet-optimized rst va vc) vd)]
    [(list (cons (neg (labelpair k p)) rst) (facet (labelpair k p) va vb)
           (facet (labelpair k p) vc vd))
     (facet (labelpair k p) vc (construct-facet-optimized rst vb vd))]
    [(list pc (facet (labelpair k p) va vb) (facet (labelpair k p) vc vd))
     #:when (label<? k (head pc))
     (facet (labelpair k p)
            (construct-facet-optimized pc va vc)
            (construct-facet-optimized pc vb vd))]
    [(list (cons (pos (labelpair k p)) rst) (facet (labelpair k p) va vb) vo)
     #:when (label<? k (head vo))
     (facet (labelpair k p) (construct-facet-optimized rst va vo) vo)]
    [(list (cons (neg (labelpair k p)) rst) (facet (labelpair k p) va vb) vo)
     #:when (label<? k (head vo))
     (facet (labelpair k p) vo (construct-facet-optimized rst vb vo))]
    [(list (cons (pos (labelpair k p)) rst) vn (facet (labelpair k p) va vb))
     #:when (label<? k (head vn))
     (facet (labelpair k p) (construct-facet-optimized rst vn va) vb)]
    [(list (cons (neg (labelpair k p)) rst) vn (facet (labelpair k p) va vb))
     #:when (label<? k (head vn))
     (facet (labelpair k p) va (construct-facet-optimized rst vn vb))]
    [(list (cons (pos (labelpair k p)) rst) vn vo)
     #:when (and (label<? k (head vn)) (label<? k (head vo)))
     (facet (labelpair k p) (construct-facet-optimized rst vn vo) vo)]
    [(list (cons (neg (labelpair k p)) rst) vn vo)
     #:when (and (label<? k (head vn)) (label<? k (head vo)))
     (facet (labelpair k p) vo (construct-facet-optimized rst vn vo))]
    [(list pc (facet (labelpair k p) va vb) vo)
     #:when (and (label<? k (head vo)) (label<? k (head pc)))
     (facet (labelpair k p)
            (construct-facet-optimized pc va vo)
            (construct-facet-optimized pc vb vo))]
    [(list pc vn (facet (labelpair k p) va vb))
     #:when (and (label<? k (head vn)) (label<? k (head pc)))
     (facet (labelpair k p)
            (construct-facet-optimized pc vn va)
            (construct-facet-optimized pc vn vb))]))

(define ((facet-fmap* f) . fvs)
  ;; Requires all faceted values to be in order (according to label<?)
  (if (ormap (match-lambda [(facet _ _ _) #t][else #f]) fvs)
      (let* ([l (foldl (lambda (fv ll) (match fv [(facet lab lv rv) (if (label<? ll lab) ll lab)][else ll])) (void) fvs)]
             [fvs+ (map (match-lambda [(and v (facet lab lv rv)) #:when (eq? lab l) v]
	                              [v (facet l v v)])
		        fvs)]
	     [lvs (map (match-lambda [(facet _ lv _) lv]) fvs+)]
	     [rvs (map (match-lambda [(facet _ lv _) lv]) fvs+)]
	     [lv (apply (facet-fmap* f) lvs)]
	     [rv (apply (facet-fmap* f) rvs)])
        (facet l lv rv))
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
