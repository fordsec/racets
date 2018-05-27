#lang reader "racets.rkt"

(require "facets.rkt")
(require racket/stxparam)

(module+ test
 (require rackunit)
 (require rackunit/text-ui)
 (check-equal?
  ((facet 'bob (lambda (x) x) (lambda (x) 1)) 3)
  (facet 'bob 3 1)))
