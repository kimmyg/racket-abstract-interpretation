#lang racket/base
(require compiler/zo-structs)

(provide static-closure)

(struct static-closure expr (lam) #:transparent)