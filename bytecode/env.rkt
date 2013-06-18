#lang racket/base
(require racket/list)

(provide empty-env
         env-set
         env-ref)

(define (empty-env)
  (make-hasheq))

(define (env-set env x v)
  (hash-set! env x v)
  env)

(define (env-ref env x)
  (hash-ref env x (λ () (error 'env-ref "binding for ~a not found" x))))

