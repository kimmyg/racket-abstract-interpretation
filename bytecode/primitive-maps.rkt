#lang racket/base
(require compiler/zo-parse
         racket/match
         racket/port
         racket/set)

(provide primitive-name->id
         primitive-id->name
         primitive-value->name
         primitive-name->value
         primitive-id->value
         primitive-value->id)

(define default-namespace
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require ''#%kernel)
    (namespace-require ''#%unsafe)
    ;(namespace-require ''#%paramz)
    ;(namespace-require ''#%foreign)
    (namespace-require ''#%futures)
    ;(namespace-require ''#%network)
    ;(namespace-require ''#%place)
    ;(namespace-require ''#%expobs)
    (namespace-require ''#%flfxnum)
    ;(namespace-require ''#%extfl)
    (current-namespace)))

(define blacklist (apply seteq '(module
                                    module*
                                  #%module-begin 
                                  #%require
                                  #%provide
                                  lambda
                                  λ
                                  define-values
                                  quote
                                  if
                                  set!
                                  #%variable-reference
                                  #%expression
                                  case-lambda
                                  let-values
                                  let*-values
                                  letrec-values
                                  begin
                                  #%stratified-body
                                  begin0
                                  unquote
                                  unquote-splicing
                                  with-continuation-mark
                                  quote-syntax
                                  define-syntaxes
                                  begin-for-syntax
                                  letrec-syntaxes+values
                                  #%app
                                  #%datum
                                  #%top)))

(define (primitive-name->id name [ns default-namespace])
  (and (not (set-member? blacklist name))
       (let ([code (compilation-top-code
                    (parameterize ([current-namespace ns])
                      (with-input-from-bytes
                       (with-output-to-bytes
                        (λ () (write (compile name))))
                       zo-parse)))])
         (and (primval? code)
              (primval-id code)))))

(define (primitive-id->name id)
  (if (hash-has-key? id->name id)
      (hash-ref id->name id)
      (error 'primitive-id->name "no name for ~a" id)))

(define (primitive-value->name value)
  (if (hash-has-key? value->name value)
      (hash-ref value->name value)
      (error 'primitive-value->name "no name for ~a" value)))

(define (primitive-name->value name [ns default-namespace])
  (and (not (set-member? blacklist name))
       (eval name ns)))

(define (primitive-id->value id [ns default-namespace])
  (match (primitive-id->name id)
    ['variable-reference->module-declaration-inspector
     (λ (_) (current-code-inspector))]
    [name
     (primitive-name->value name ns)]))

(define (primitive-value->id value [ns default-namespace])
  (primitive-name->id (primitive-value->name value) ns))

(define-values (id->name value->name)
  (for/fold ([id->name (hasheq)]
             [value->name (hasheq)])
    ([name (namespace-mapped-symbols default-namespace)])
    (values (hash-set id->name (primitive-name->id name) name)
            (hash-set value->name (primitive-name->value name) name))))
