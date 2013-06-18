#lang racket/base
(require racket/list
         racket/match
         racket/vector
         compiler/demodularizer/main
         compiler/zo-parse
         compiler/zo-structs
         "load.rkt"
         "primitive-maps.rkt"
         "static-closure.rkt")

#;(parameterize ([garbage-collect-toplevels-enabled #t])
    (demodularize "../tests/capturing-inner-define.rkt"))

(define bc (with-input-from-file "../tests/capturing-inner-define_rkt_merged.zo" zo-parse))

(define fresh
  (let ([counts (make-hasheq)])
    (λ (base)
      (begin0
        (string->symbol (format "~a~a" base (hash-ref counts base 0)))
        (hash-update! counts base add1 0)))))

(struct def-values* form (ids rhs) #:transparent)

(struct closure* expr (code gen-id) #:transparent)
(struct lam* expr (name flags params param-types rest? closure-map closure-types toplevel-map max-let-depth body) #:transparent)
(struct localref* expr (unbox? id clear? other-clears? type) #:transparent)
(struct primval* expr (id) #:transparent)
(struct toplevel* expr (id const? ready?) #:transparent)

(define (recover e [stack empty])
  (cond
    [(list? e)
     (map (λ (e) (recover e stack)) e)]
    [(zo? e)
     (match e
       [(application rator rands)
        (let ([stack (append (build-list (length rands) (λ (_) (fresh 'x))) stack)])
          (application (recover rator stack)
                       (recover rands stack)))]
       [(apply-values proc args-expr)
        (apply-values (recover proc stack)
                      (recover args-expr stack))]
       [(branch test then else)
        (branch (recover test stack)
                (recover then stack)
                (recover else stack))]
       [(closure code gen-id)
        (closure* (recover code stack) gen-id)]
       [(compilation-top max-let-depth prefix′ code)
        (match-let ([(prefix num-lifts toplevels syntaxes) prefix′])
          (let* ([toplevels (map (λ (x) (or x (fresh 'toplevel))) toplevels)]
                 [prefix′ (prefix num-lifts toplevels syntaxes)])
            (compilation-top max-let-depth prefix′ (recover code (cons toplevels stack)))))]
       [(def-values ids rhs)
        (def-values* (recover ids stack)
                     (recover rhs stack))]
       [(lam name flags num-params param-types rest? closure-map
             closure-types toplevel-map max-let-depth body)
        (let* ([params (build-list num-params (λ (_) (fresh 'param)))]
               [rest (if rest? (list (fresh 'rest)) empty)]
               [closure-map′ (vector-map (λ (i) (list-ref stack i)) closure-map)]
               [stack′ stack]
               [stack′ (append rest stack′)]
               [stack′ (append params stack′)]
               [stack′ (append (vector->list closure-map′) stack′)])
          (lam* name flags params param-types (and rest? (first rest)) closure-map′ closure-types toplevel-map max-let-depth
                (recover body stack′)))]
       [(localref unbox? pos clear? other-clears? type)
        (localref* unbox? (list-ref stack pos) clear? other-clears? type)]
       [(mod name srcname self-modidx prefix′ provides requires body
             syntax-bodies unexported max-let-depth dummy lang-info
             internal-context flags pre-submodules post-submodules)
        (match-let ([(prefix num-lifts toplevels syntaxes) prefix′])
          (let* ([toplevels (map (λ (x) (or x (fresh 'toplevel))) toplevels)]
                 [prefix′ (prefix num-lifts toplevels syntaxes)])
            (mod name srcname self-modidx prefix′ provides requires
                 (recover body (cons toplevels stack))
                 syntax-bodies unexported max-let-depth dummy lang-info
                 internal-context flags pre-submodules post-submodules)))]
       [(primval id)
        (primval* (primitive-id->name id))]
       [(seq forms)
        (seq (recover forms stack))]
       [(static-closure lam)
        (static-closure (recover lam stack))]
       [(static-ref id)
        (static-ref id)]
       [(toplevel depth pos const? ready?)
        (toplevel* (list-ref (list-ref stack depth) pos) const? ready?)]
       [f
        (error 'run "~a" f)])]
    [else
     e]))


(let-values ([(body closure-map) (load bc)])
  (values (recover body)
          (for/fold ([closure-map (hasheq)])
            ([(id closure) (in-hash closure-map)])
            (hash-set closure-map id (recover closure)))))
