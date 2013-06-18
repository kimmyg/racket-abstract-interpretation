#lang racket/base
(require compiler/zo-structs)

(provide static-closure
         static-ref)

(struct static-closure expr (lam) #:transparent)
(struct static-ref expr (id) #:transparent)