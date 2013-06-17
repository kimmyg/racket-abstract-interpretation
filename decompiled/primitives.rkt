#lang racket/base

(provide known-primitive?
         primitive-ref)

(define primitives
  (hasheq '* *
          'apply apply
          'current-print current-print
          'for-each for-each
          'sub1 sub1
          'values values
          'zero? zero?))

(define (known-primitive? x)
  (hash-has-key? primitives x))

(define (primitive-ref x)
  (hash-ref primitives x))
