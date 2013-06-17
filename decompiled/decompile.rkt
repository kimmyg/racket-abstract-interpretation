#lang racket/base
(require [prefix-in racket: compiler/decompile]
         compiler/demodularizer/main
         compiler/zo-parse)

(provide decompile)

(define (decompile input-path [output-path #f])
  (define-values (base name must-be-dir?) (split-path input-path))
  (when must-be-dir?
    (error 'cfa "Cannot be run on directory"))

  (define merged-zo-path
    (path-add-suffix input-path "_merged.zo"))
  
  (define analyzed-zo-path
    (or output-path (path-add-suffix input-path "_analyzed.zo")))
  
  (parameterize ([garbage-collect-toplevels-enabled #t])
    (demodularize input-path merged-zo-path))
  
  (define merged-zo
    (with-input-from-file merged-zo-path zo-parse))
  
  (racket:decompile merged-zo))