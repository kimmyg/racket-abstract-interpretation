#lang racket/base
(require compiler/zo-structs)

(provide (struct-out static-closure)
         (struct-out static-ref))

(struct static-closure expr (lam) #:transparent)
(struct static-ref expr (id) #:transparent)