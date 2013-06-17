#lang racket/base

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))

(fact 17)