#lang racket/base
(require racket/list
         racket/match
         racket/vector
         (prefix-in s: "shadow-zo-structs")
         "interpreter-structs.rkt"
         "primitive-maps.rkt")

(provide preprocess)

(define fresh
  (let ([counts (make-hasheq)])
    (λ (base)
      (begin0
        (string->symbol (format "~a~a" base (hash-ref counts base 0)))
        (hash-update! counts base add1 0)))))


[(def-values ids rhs)
 (s:def-values (map inner ids) (inner rhs))]
[(seq forms)
 (s:seq (map inner forms))]
[(mod name srcname self-modidx prefix provides requires
      body syntax-bodies unexported max-let-depth dummy
      lang-info internal-context flags pre-submodules
      post-submodules)
 (s:mod name srcname self-modidx (inner prefix) provides requires
        (map inner body) syntax-bodies unexported max-let-depth
        dummy lang-info internal-context flags pre-submodules
        post-submodules)]


[(toplevel depth pos const? ready?)
 (s:toplevel depth pos const? ready?)]




(define (preprocess zo indirect-map)
  (define (inner e stack)
    (cond
      [(list? e)
       (map (λ (e) (inner e stack)) e)]
      [(zo? e)
       (match e
         [(indirect id)
          (i:indirect id)]
         [(s:application rator rands)
          (let ([stack (append (build-list (length rands) (λ (_) (fresh 'x))) stack)])
            (application (inner rator stack)
                         (inner rands stack)))]
         [(r:branch test then else)
          (branch (inner test stack)
                  (inner then stack)
                  (inner else stack))]
         [(r:closure code gen-id)
          (closure (inner code stack) gen-id)]
         [(r:def-values ids rhs)
          (def-values (inner ids stack)
            (inner rhs stack))]
         [(indirect id)
          (indirect id)]
         [(lam name flags num-params param-types rest? closure-map
               closure-types toplevel-map max-let-depth body)
          (let* ([params (build-list num-params (λ (_) (fresh 'param)))]
                 [rest (if rest? (list (fresh 'rest)) empty)]
                 [closed-ids (vector-map (λ (i) (list-ref stack i)) closure-map)]
                 [stack′ stack]
                 [stack′ (append rest stack′)]
                 [stack′ (append params stack′)]
                 [stack′ (append (vector->list closed-ids) stack′)])
            (lam params (and rest? (first rest)) (inner body stack′)))]
         [(localref unbox? pos clear? other-clears? type)
          (i:localref (list-ref stack pos))]
         [(seq forms)
          (i:seq (inner forms stack))]
         [(closure lam)
          (i:closure (inner lam stack))]
         
         
         
         [(lam name flags num-params param-types rest? closure-map
               closure-types toplevel-map max-let-depth body)
          (let* ([params (build-list num-params (λ (_) (fresh 'param)))]
                 [rest (if rest? (list (fresh 'rest)) empty)]
                 [closed-ids (vector-map (λ (i) (list-ref stack i)) closure-map)]
                 [stack′ stack]
                 [stack′ (append rest stack′)]
                 [stack′ (append params stack′)]
                 [stack′ (append (vector->list closed-ids) stack′)])
            (i:lam params (and rest? (first rest)) (inner body stack′)))]]
         [(closure code gen-id)
          (i:closure (inner code stack))]
         [(case-lam name clauses)
          (i:case-lam (inner clauses stack))]
         [(let-one rhs body type unused?)
          (let* ([one (fresh 'one)]
                 [stack′ (cons one stack)])
            (i:let-one one (inner rhs stack′) (inner body stack′)))]
         [(let-void count boxes? body)
          (let ([slots (make-list count (λ (_) (fresh 'void)))])
            (i:let-void slots (inner body (append slots stack))))]
         [(install-value count pos boxes? rhs body)
          (i:install-value (take (drop stack pos) count) (inner rhs stack) (inner body stack))]
         [(let-rec procs body)
          (s:let-rec (inner procs stack) (inner body stack))]
         [(boxenv pos body)
          (i:boxenv (list-ref stack pos) (inner body stack))]
         [(localref unbox? pos clear? other-clears? type)
          (i:localref (list-ref stack pos))]
         
         [(toplevel depth pos const? ready?)
          (i:toplevel (list-ref (list-ref stack depth) pos))]
         [(application rator rands)
          (s:application (inner rator stack) (inner rands stack))]
         [(branch test then else)
          (s:branch (inner test stack) (inner then stack) (inner else stack))]
         [(with-cont-mark key val body)
          (i:with-cont-mark (inner key stack) (inner val stack) (inner body stack))]
         [(beg0 seq)
          (i:beg0 (map inner seq))]
         [(assign id rhs undef-ok?)
          (i:assign (inner id stack) (inner rhs stack))]
         [(apply-values proc args-expr)
          (i:apply-values (inner proc stack) (inner args-expr stack))]
         [(primval id)
          (i:primval (primitive-id->name id))]
         [f
          (error 'preprocess "~a" f)])]
      [else
       e]))
  (match e
    [(compilation-top top-max-let-depth (prefix top-num-lifts top-tls top-syntaxes)
                      (mod name srcname self-modidx (prefix mod-num-lifts mod-tls mod-syntaxes)
                           provides requires body syntax-bodies unexported mod-max-let-depth dummy
                           lang-info internal-context flags pre-submodules post-submodules))
     (let ([recover-name (λ (x) (or x (fresh 'toplevel)))])
       (let ([top-tls (map recover-name top-tls)]
             [mod-tls (map recover-name mod-tls)])
         (values (splice (inner body (list mod-tls top-tls))) (append mod-tls top-tls))))]
    [(? closure?)
     (values (inner e empty) empty)]))
