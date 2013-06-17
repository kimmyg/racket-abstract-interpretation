#lang racket/base
(require racket/match
         (prefix-in e: "forms.rkt")
         "static-closure.rkt"
         "env.rkt"
         "store.rkt")

(provide load)

(define (load e)
  (define static-closures (make-hasheq))
  (define inner
    (match-lambda
      [(e:application fun args)
       (e:application (inner fun) (map inner args))]
      [(e:apply-values proc expr)
       (e:apply-values (inner proc) (inner expr))]
      [(e:begin es)
       (e:begin (map inner es))]
      [(e:closed name lambda)
       (hash-set! static-closures name (static-closure (inner lambda)))
       (e:ref name)]
      [(e:define-values ids rhs)
       (e:define-values ids (inner rhs))]
      [(e:if test-expr con-expr alt-expr)
       (e:if (inner test-expr) (inner con-expr) (inner alt-expr))]
      [(e:lambda context flags params rest body)
       (e:lambda context flags params rest (inner body))]
      [(e:module name language es)
       (e:module name language (map inner es))]
      [(and e (e:quote _))
       e]
      [(and e (e:ref _))
       e]
      [(and e (e:require _))
       e]
      [(e:sfs-clear e)
       (e:sfs-clear (inner e))]))
  (let*-values ([(e) (inner e)]
                [(env store)
                 (for/fold ([env empty-env]
                            [store empty-store])
                   ([(id v) (in-hash static-closures)])
                   (let-values ([(store addr) (store-set store v)])
                     (values (env-set env id addr) store)))])
    (values e env store)))

