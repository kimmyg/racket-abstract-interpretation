#lang racket/base
(require racket/list
         "primitives.rkt")

(provide empty-env
         env-set
         env-ref)

(define empty-env (hasheq))

(define env-set hash-set)

(define (env-ref env x)
  (cond
    [(hash-has-key? env x)
     (hash-ref env x)]
    [(known-primitive? x)
     x]
    [else
     (error 'env-ref "binding for ~a not found" x)]))

