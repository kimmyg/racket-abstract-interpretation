#lang racket/base
(require (only-in "forms.rkt" expr))

(provide static-closure)

(struct static-closure expr (lambda) #:transparent)