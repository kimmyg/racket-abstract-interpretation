#lang racket/base
(require racket/list
         racket/match
         racket/vector
         compiler/zo-structs
         "interpreter-structs.rkt"
         "primitive-maps.rkt"
         "static-closure.rkt")

(provide recover-names)

(define fresh
  (let ([counts (make-hasheq)])
    (λ (base)
      (begin0
        (string->symbol (format "~a~a" base (hash-ref counts base 0)))
        (hash-update! counts base add1 0)))))


(define (recover-names e)
  (define (inner e stack)
    (cond
      [(list? e)
       (map (λ (e) (inner e stack)) e)]
      [(zo? e)
       (match e
         [(application rator rands)
          (let ([stack (append (build-list (length rands) (λ (_) (fresh 'x))) stack)])
            (application (inner rator stack)
                         (inner rands stack)))]
         [(apply-values proc args-expr)
          (apply-values (inner proc stack)
                        (inner args-expr stack))]
         [(branch test then else)
          (branch (inner test stack)
                  (inner then stack)
                  (inner else stack))]
         [(closure code gen-id)
          (closure* (inner code stack) gen-id)]
         [(compilation-top max-let-depth prefix′ code)
          (match-let ([(prefix num-lifts toplevels syntaxes) prefix′])
            (let* ([toplevels (map (λ (x) (or x (fresh 'toplevel))) toplevels)]
                   [prefix′ (prefix num-lifts toplevels syntaxes)])
              (compilation-top max-let-depth prefix′ (inner code (cons toplevels stack)))))]
         [(def-values ids rhs)
          (def-values* (inner ids stack)
            (inner rhs stack))]
         [(lam name flags num-params param-types rest? closure-map
               closure-types toplevel-map max-let-depth body)
          (let* ([params (build-list num-params (λ (_) (fresh 'param)))]
                 [rest (if rest? (list (fresh 'rest)) empty)]
                 [closed-ids (vector-map (λ (i) (list-ref stack i)) closure-map)]
                 [stack′ stack]
                 [stack′ (append rest stack′)]
                 [stack′ (append params stack′)]
                 [stack′ (append (vector->list closed-ids) stack′)])
            (lam* name flags params param-types (and rest? (first rest))
                  closed-ids max-let-depth (inner body stack′)))]
         [(localref unbox? pos clear? other-clears? type)
          (localref* unbox? (list-ref stack pos) clear? other-clears? type)]
         [(mod name srcname self-modidx prefix′ provides requires body
               syntax-bodies unexported max-let-depth dummy lang-info
               internal-context flags pre-submodules post-submodules)
          (match-let ([(prefix num-lifts toplevels syntaxes) prefix′])
            (let* ([toplevels (map (λ (x) (or x (fresh 'toplevel))) toplevels)]
                   [prefix′ (prefix num-lifts toplevels syntaxes)])
              (mod name srcname self-modidx prefix′ provides requires
                   (inner body (cons toplevels stack))
                   syntax-bodies unexported max-let-depth dummy lang-info
                   internal-context flags pre-submodules post-submodules)))]
         [(primval id)
          (primval* (primitive-id->name id))]
         [(seq forms)
          (seq (inner forms stack))]
         [(static-closure lam)
          (static-closure (inner lam stack))]
         [(static-ref id)
          (static-ref id)]
         [(toplevel depth pos const? ready?)
          (toplevel* (list-ref (list-ref stack depth) pos) const? ready?)]
         [f
          (error 'run "~a" f)])]
      [else
       e]))
  (inner e empty))
