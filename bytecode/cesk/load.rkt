#lang racket/base
(require "decycle.rkt"
         "preprocess.rkt"
         "env.rkt"
         "store.rkt")

(provide load
         (struct-out undefined))

(struct undefined ())

(define (load e)
  (let*-values ([(con closure-map) (decycle e)]
                [(con names) (preprocess con)]
                [(closure-map) (for/fold ([closure-map (hasheq)])
                                 ([(id closure) (in-hash closure-map)])
                                 (let-values ([(closure names) (preprocess closure)])
                                   (hash-set closure-map id closure)))]
                [(env str) (for/fold ([env (empty-env)]
                                      [str (empty-store)])
                             ([(id closure) (in-hash closure-map)])
                             (let*-values ([(str addr) (store-set str closure)]
                                           [(env) (env-set env id addr)])
                               (values env str)))]
                [(env str) (for/fold ([env env]
                                      [str str])
                             ([name names])
                             (let*-values ([(str addr) (store-set str (undefined))]
                                           [(env) (env-set env name addr)])
                               (values env str)))])
    (values con env str)))
