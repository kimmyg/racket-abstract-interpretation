#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs
         "static-closure.rkt")

(provide decycle)

(define (decycle e)
  (define seen-closures (seteq))
  (define addr->closure (make-hasheq))
  (define (inner e)
    (if (zo? e)
        (match e
          [(application rator rands)
           (application (inner rator) (map inner rands))]
          [(apply-values proc args-expr)
           (apply-values (inner proc) (inner args-expr))]
          [(branch test then else)
           (branch (inner test) (inner then) (inner else))]
          [(and clo (closure code id))
           (unless (set-member? seen-closures clo)
             (set! seen-closures (set-add seen-closures clo))
             (hash-set! addr->closure id (static-closure (inner code))))
           (static-ref id)]
          [(compilation-top max-let-depth prefix code)
           (compilation-top max-let-depth prefix (inner code))]
          [(def-values ids rhs)
           (def-values ids (inner rhs))]
          [(lam name flags num-params param-types rest? closure-map
                closure-types toplevel-map max-let-depth body)
           (lam name flags num-params param-types rest? closure-map
                closure-types toplevel-map max-let-depth (inner body))]
          [(localref unbox? pos clear? other-clears? type)
           (localref unbox? pos clear? other-clears? type)]
          [(mod name srcname self-modidx prefix provides requires body
                syntax-bodies unexported max-let-depth dummy lang-info
                internal-context flags pre-submodules post-submodules)
           (mod name srcname self-modidx prefix provides requires (map inner body)
                syntax-bodies unexported max-let-depth dummy lang-info
                internal-context flags pre-submodules post-submodules)]
          [(prefix num-lifts toplevels stxs)
           (prefix num-lifts toplevels stxs)]
          [(primval id)
           (primval id)]
          [(seq forms)
           (seq (map inner forms))]
          [(toplevel depth pos const? ready?)
           (toplevel depth pos const? ready?)])
        e))
  (values (inner e) addr->closure))
