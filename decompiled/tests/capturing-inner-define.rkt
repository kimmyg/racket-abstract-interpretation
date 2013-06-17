#lang racket/base

(define (outer n true)
  (define (even? n)
    (if (zero? n)
        true
        (odd? (sub1 n))))
  (define (odd? n)
    (if (zero? n)
        (not true)
        (even? (sub1 n))))
  (even? n))

(outer 10 #t)
