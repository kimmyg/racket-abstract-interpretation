#lang racket/base

(define (f xs)
  (define l (length xs))
  (define m (length (reverse xs)))
  (+ l m))

(f (list 1 2 3))
