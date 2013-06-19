#lang racket/base
(require racket/list
         racket/match
         racket/vector
         [prefix-in r: compiler/zo-structs]
         "interpreter-structs.rkt"
         "primitive-maps.rkt"
         "static-closure.rkt")

(provide preprocess)

(define fresh
  (let ([counts (make-hasheq)])
    (λ (base)
      (begin0
        (string->symbol (format "~a~a" base (hash-ref counts base 0)))
        (hash-update! counts base add1 0)))))


(define (preprocess e)
  (define (inner e stack)
    (cond
      [(list? e)
       (map (λ (e) (inner e stack)) e)]
      [(r:zo? e)
       (match e
         [(r:application rator rands)
          (let ([stack (append (build-list (length rands) (λ (_) (fresh 'x))) stack)])
            (application (inner rator stack)
                         (inner rands stack)))]
         [(r:apply-values proc args-expr)
          (apply-values (inner proc stack)
                        (inner args-expr stack))]
         [(r:branch test then else)
          (branch (inner test stack)
                  (inner then stack)
                  (inner else stack))]
         [(r:closure code gen-id)
          (closure (inner code stack) gen-id)]
         [(r:def-values ids rhs)
          (def-values (inner ids stack)
                      (inner rhs stack))]
         [(r:lam name flags num-params param-types rest? closure-map
                 closure-types toplevel-map max-let-depth body)
          (let* ([params (build-list num-params (λ (_) (fresh 'param)))]
                 [rest (if rest? (list (fresh 'rest)) empty)]
                 [closed-ids (vector-map (λ (i) (list-ref stack i)) closure-map)]
                 [stack′ stack]
                 [stack′ (append rest stack′)]
                 [stack′ (append params stack′)]
                 [stack′ (append (vector->list closed-ids) stack′)])
            (lam params (and rest? (first rest)) (inner body stack′)))]
         [(r:localref unbox? pos clear? other-clears? type)
          (localref (list-ref stack pos))]
         [(r:primval id)
          (primval (primitive-id->name id))]
         [(r:seq forms)
          (seq (inner forms stack))]
         [(static-closure lam)
          (static-closure (inner lam stack))]
         [(static-ref id)
          (static-ref id)]
         [(r:toplevel depth pos const? ready?)
          (toplevel (list-ref (list-ref stack depth) pos))]
         [f
          (error 'preprocess "~a" f)])]
      [else
       e]))
  (match e
    [(r:compilation-top top-max-let-depth (r:prefix top-num-lifts top-tls top-syntaxes)
                        (r:mod name srcname self-modidx (r:prefix mod-num-lifts mod-tls mod-syntaxes)
                               provides requires body syntax-bodies unexported mod-max-let-depth dummy
                               lang-info internal-context flags pre-submodules post-submodules))
     (let ([assign-name (λ (x) (or x (fresh 'toplevel)))])
       (let ([top-tls (map assign-name top-tls)]
             [mod-tls (map assign-name mod-tls)])
         (values (splice (inner body (list mod-tls top-tls))) (append mod-tls top-tls))))]
    [(? static-closure?)
     (values (inner e empty) empty)]))
