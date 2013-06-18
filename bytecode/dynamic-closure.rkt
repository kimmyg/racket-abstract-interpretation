#lang racket/base
(require compiler/zo-structs)

(provide dynamic-closure)

(struct dynamic-closure expr (lam env) #:transparent)