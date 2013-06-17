#lang racket/base
(require (only-in "forms.rkt" expr))

(provide dynamic-closure)

(struct dynamic-closure expr (lambda env) #:transparent)