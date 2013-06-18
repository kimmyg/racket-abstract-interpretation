#lang racket/base
(require compiler/zo-parse
         "load.rkt"
         "recover-names.rkt"
         "env.rkt"
         "store.rkt"
         "interp.rkt")

(define bc (with-input-from-file "../tests/fact_rkt_merged.zo" zo-parse))


(let*-values ([(body closure-map) (load bc)]
              [(body) (recover-names body)]
              [(closure-map) (for/fold ([closure-map (hasheq)])
                               ([(id closure) (in-hash closure-map)])
                               (hash-set closure-map id (recover-names closure)))]
              [(env str) (for/fold ([env (empty-env)]
                                    [str (empty-store)])
                           ([(id closure) (in-hash closure-map)])
                           (let*-values ([(str addr) (store-set str closure)]
                                         [(env) (env-set env id addr)])
                             (values env str)))])
  (interp body env str))

#;(parameterize ([garbage-collect-toplevels-enabled #t])
    (demodularize "../tests/capturing-inner-define.rkt"))
