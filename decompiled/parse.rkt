#lang racket/base
(require racket/match
         "forms.rkt")

(provide parse)

(define parse
  (match-lambda
    [(? symbol? x)
     x]
    [`(if ,test-expr ,con-expr ,alt-expr)
     (if (parse test-expr) (parse con-expr) (parse alt-expr))]
    [`(begin ,es ...)
     (begin (map parse es))]
    [`(module ,name ,language ,forms ...)
     (module name language (map parse forms))]
    [`(quote ,e)
     e] ; this should be special
    [`(require ,forms ...)
     (require (map parse forms))]
    [`(define-values ,ids ,rhs)
     (define-values (map parse ids) (parse rhs))]
    [`(#%closed ,name ,lambda)
     (closed name (parse lambda))]
    [(or `(lambda ,param-spec ,context ,flags ,body)
         `(lambda ,param-spec ,context ,flags ,_ ,body))
     (lambda context flags (param-ids param-spec) (rest-id param-spec) (parse body))]
    [`(#%apply-values ,procedure ,expression)
     (apply-values (parse procedure) (parse expression))]
    [`(,function ,arguments ...)
     (application (parse function) (map parse arguments))]
    [e (error 'parse "no match for ~a" e)]))
