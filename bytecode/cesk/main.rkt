#lang racket/base
(require compiler/zo-parse
         "decycle.rkt"
         "interp.rkt"
         "load.rkt")

(define (load-and-interp path)
  (let ([bc (with-input-from-file path zo-parse)])
    ((compose interp load) bc)))

;(decycle (with-input-from-file "../tests/fact_rkt_merged.zo" zo-parse))
;(decycle (with-input-from-file "../tests/capturing-inner-define_rkt_merged.zo" zo-parse))
;(decycle (with-input-from-file "../tests/rsa_rkt_merged.zo" zo-parse))

;(load-and-interp "../tests/fact_rkt_merged.zo")
(load-and-interp "../tests/capturing-inner-define_rkt_merged.zo")
;(load-and-interp "../tests/length_rkt_merged.zo")
;(load-and-interp "../tests/rsa_rkt_merged.zo")


; get bytecode
; pull out closed lambdas (introducing static-ref and static-closure) (bc -> con* map*)
; perform renames and change to interpreter bytecode (con* map* -> con env str)
; interp con env str


#;(parameterize ([garbage-collect-toplevels-enabled #t])
    (demodularize "../tests/capturing-inner-define.rkt"))
