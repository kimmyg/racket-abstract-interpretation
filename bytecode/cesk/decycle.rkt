#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs
         (prefix-in s: "shadow-zo-structs.rkt")
         "zo-cycles.rkt")

(provide decycle)

(define (decycle zo)
  (define fresh (let ([i 0])
                  (λ ()
                    (begin0
                      (string->symbol (format "x~a" i))
                      (set! i (add1 i))))))
  (define cycles (cycle-points zo))
  (define zo->indirect (make-hasheq)) ; native zos
  (define indirect->zo (make-hasheq)) ; shadow zos
  (define (inner zo)
    (if (zo? zo)
        (if (set-member? cycles zo)
            (begin
              (unless (hash-has-key? zo->indirect zo)
                (let ([indirect (s:indirect (fresh))])
                  (hash-set! zo->indirect zo indirect)
                  (hash-set! indirect->zo indirect (transform zo))
                  (hash-ref zo->indirect zo)))
              (hash-ref zo->indirect zo))
            (transform zo))
        zo))
  (define transform
    (match-lambda
      [(compilation-top max-let-depth prefix code)
       (s:compilation-top max-let-depth (inner prefix) (inner code))]
      [(prefix num-lifts toplevels stxs)
       (s:prefix num-lifts (map inner toplevels) stxs)]
      
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
      
      [(lam name flags num-params param-types rest?
            closure-map closure-types toplevel-map
            max-let-depth body)
       (s:lam name flags num-params param-types rest?
              closure-map closure-types toplevel-map
              max-let-depth (inner body))]
      [(closure code gen-id)
       (s:closure (inner code) gen-id)]
      [(case-lam name clauses)
       (s:case-lam name (map inner clauses))]
      [(let-one rhs body type unused?)
       (s:let-one rhs (inner body) type unused?)]
      [(let-void count boxes? body)
       (s:let-void count boxes? (inner body))]
      [(install-value count pos boxes? rhs body)
       (s:install-value count pos boxes? (inner rhs) (inner body))]
      [(let-rec procs body)
       (s:let-rec (map inner procs) (inner body))]
      [(boxenv pos body)
       (s:boxenv pos (inner body))]
      [(localref unbox? pos clear? other-clears? type)
       (s:localref unbox? pos clear? other-clears? type)]
      [(toplevel depth pos const? ready?)
       (s:toplevel depth pos const? ready?)]
      [(topsyntax depth pos midpt)
       (s:topsyntax depth pos midpt)]
      [(application rator rands)
       (s:application (inner rator) (map inner rands))]
      [(branch test then else)
       (s:branch (inner test) (inner then) (inner else))]
      [(with-cont-mark key val body)
       (s:with-cont-mark (inner key) (inner val) (inner body))]
      [(beg0 seq)
       (s:beg0 (map inner seq))]
      [(varref toplevel dummy)
       (s:varref toplevel dummy)]
      [(assign id rhs undef-ok?)
       (s:assign (inner id) (inner rhs) undef-ok?)]
      [(apply-values proc args-expr)
       (s:apply-values (inner proc) (inner args-expr))]
      [(primval id)
       (s:primval id)]))
  (let ([zo (inner zo)]
        [indirect-map (for/fold ([map (hasheq)])
                        ([(indirect zo) (in-hash indirect->zo)])
                        (hash-set map (s:indirect-id indirect) zo))])
    (values zo indirect-map)))