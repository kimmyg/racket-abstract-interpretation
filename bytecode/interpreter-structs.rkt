#lang racket/base
(require (only-in compiler/zo-structs
                  form
                  expr
                  application
                  apply-values
                  branch
                  seq
                  splice))

(provide (all-from-out compiler/zo-structs)
         def-values
         closure
         lam
         localref
         primval
         (struct-out toplevel))

(struct def-values form (ids rhs) #:transparent)

(struct closure expr (code id) #:transparent)
(struct lam expr (params rest body) #:transparent)
(struct localref expr (id) #:transparent)
(struct primval expr (id) #:transparent)
(struct toplevel expr (id) #:transparent)
