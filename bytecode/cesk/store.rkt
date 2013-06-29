#lang racket/base
(require racket/list)

(provide empty-store
         store-set
         store-ref)

(define (empty-store)
  (hasheq))

(define fresh-addr
  (let ([i 0])
    (λ ()
      (begin0
        (string->symbol (format "addr~a" i))
        (set! i (add1 i))))))

(define (store-set store v [x (fresh-addr)])
  (values (hash-set store x v) x))

(define (store-ref store x)
  (hash-ref store x (λ () (error 'store-ref "no value for address ~a" x))))

