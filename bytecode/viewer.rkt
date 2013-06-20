#lang racket/base
(require racket/match
         compiler/demodularizer/main
         compiler/zo-parse)

#;(thread
 (λ ()
   (letrec ([loop (λ (receiver)
                    (match (sync receiver)
                      [a
                       (printf "~a" a)]))])
     (loop (make-log-receiver (current-logger)
                              'debug
                              'demodularizer)))))

(unless (directory-exists? "tests")
  (make-directory "tests"))

(define view
  (match-lambda
    [(and mod
          `(module ,name ,language ,forms ...))
     (let ([source-filename (format "tests/~a.rkt" name)]
           [demodularized-filename (string->path (format "tests/~a_rkt_merged.zo" name))])
       (unless (file-exists? source-filename)
         (with-output-to-file source-filename
           (λ () (write mod))))
       (parameterize ([compile-context-preservation-enabled #t]
                      [garbage-collect-toplevels-enabled #t])
         (demodularize source-filename demodularized-filename))
       (with-input-from-file demodularized-filename zo-parse))]
    [e
     (error 'view "expected a module expression; got ~a" e)]))

(view '(module recursive-lambda racket/base
         (define f
           (case-lambda
             [(n) (f n 1)]
             [(n a) (if (zero? n)
                        a
                        (f (sub1 n) (* a n)))]))
         (f 12)))

