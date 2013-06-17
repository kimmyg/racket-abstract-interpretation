#lang racket/base
(require racket/list
         racket/match)

(provide expr
         ref
         if
         begin
         module
         quote
         require
         define-values
         closed
         lambda
         apply-values
         application
         sfs-clear
         param-ids
         rest-id)

(struct expr () #:transparent)
(struct ref expr (id) #:transparent)
(struct if expr (test-expr con-expr alt-expr) #:transparent)
(struct begin expr (es) #:transparent)
(struct module expr (name language forms) #:transparent)
(struct quote expr (e) #:transparent)
(struct require expr (forms) #:transparent)
(struct define-values expr (ids rhs) #:transparent)
(struct closed expr (name lambda) #:transparent)
(struct lambda expr (context flags params rest body) #:transparent)
(struct apply-values expr (procedure expression) #:transparent)
(struct application expr (function arguments) #:transparent)
(struct sfs-clear expr (expression) #:transparent)

(define param-ids
  (match-lambda
    [(? symbol?)
     empty]
    [(list)
     empty]
    [(cons id params-spec)
     (cons id (param-ids params-spec))]))

(define (rest-id params-spec)
  (cond
    [(symbol? params-spec)
     params-spec]
    [(empty? params-spec)
     #f]
    [(pair? params-spec)
     (rest-id (cdr params-spec))]))
