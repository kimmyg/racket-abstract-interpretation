#lang racket/base
(require racket/list)

(provide empty-env
         env-set
         env-ref)

(define (empty-env)
  (hasheq))

(define env-set hash-set)

(define (env-ref env x)
  (hash-ref env x (λ () (error 'env-ref "binding for ~a not found" x))))

