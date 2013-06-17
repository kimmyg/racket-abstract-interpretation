#lang racket/base
(require racket/match
         "decompile.rkt"
         "interp.rkt"
         "parse.rkt")

(parameterize ([compile-context-preservation-enabled #t])
  (interp (parse (decompile "tests/fact.rkt"))))
