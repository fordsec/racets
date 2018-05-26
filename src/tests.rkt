#lang reader "racets.rkt"

(require "facets.rkt")

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (check-eq?
   ((facet 'bob (lambda (x) x) (lambda (x) 1)) 3)
   (facet 'bob 3 1)))
