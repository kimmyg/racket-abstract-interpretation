#lang racket/base
(require racket/match
         "forms.rkt")

(provide parse)

(define parse
  (match-lambda
    [`(#%apply-values ,procedure ,expression)
     (apply-values (parse procedure) (parse expression))]
    [`(begin ,es ...)
     (begin (map parse es))]
    [`(#%closed ,name ,lambda)
     (closed name (parse lambda))]
    [`(define-values ,ids ,rhs)
     (define-values ids (parse rhs))]
    [`(if ,test-expr ,con-expr ,alt-expr)
     (if (parse test-expr) (parse con-expr) (parse alt-expr))]
    #;[`(lambda ,param-spec ,body)
     ;(displayln "lambda 0")
     (lambda #f #f (param-ids param-spec) (rest-id param-spec) (parse body))]
    [`(lambda ,param-spec ,context ,body)
     ;(displayln "lambda 1")
     (lambda context #f (param-ids param-spec) (rest-id param-spec) (parse body))]
    [`(lambda ,param-spec ,context ,flags ,body)
     ;(displayln "lambda 2")
     (lambda context flags (param-ids param-spec) (rest-id param-spec) (parse body))]
    [`(lambda ,param-spec ,context ,flags ,unknown ,body)
     ;(printf "lambda 3; unknown is ~a\n" unknown)
     (lambda context flags (param-ids param-spec) (rest-id param-spec) (parse body))]
    [`(module ,name ,language ,forms ...)
     (module name language (map parse forms))]
    [`(quote ,e)
     (quote e)] ; this should be special
    [`(require ,forms ...)
     (require (map parse forms))]
    [`(#%sfs-clear ,e)
     (sfs-clear (parse e))]
    [`(,function ,arguments ...)
     (application (parse function) (map parse arguments))]
    [(? symbol? x)
     (ref x)]
    [e (error 'parse "no match for ~a" e)]))
