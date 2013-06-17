#lang racket/base
(require racket/match
         "decompile.rkt"
         "interp.rkt"
         "load.rkt"
         "parse.rkt")

(parameterize ([compile-context-preservation-enabled #t])
  (let-values ([(con env str) (load (parse (decompile "tests/capturing-inner-define.rkt")))])
    (interp con env str)))
