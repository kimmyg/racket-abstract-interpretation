#lang racket/base
(require compiler/zo-structs)

(provide def-values*
         
         closure*
         lam*
         localref*
         primval*
         toplevel*
         toplevel*-id)

(struct def-values* form (ids rhs) #:transparent)

(struct closure* expr (code gen-id) #:transparent)
(struct lam* expr (name flags params param-types rest? closed-ids max-let-depth body) #:transparent)
(struct localref* expr (unbox? id clear? other-clears? type) #:transparent)
(struct primval* expr (id) #:transparent)
(struct toplevel* expr (id const? ready?) #:transparent)
