#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs)

(provide cycle-points)

(define (cycle-points e)
  (define ((inner seen) e)
    (if (zo? e)
        (if (set-member? seen e)
            (seteq e)
            (let ([inner0 (inner (set-add seen e))])
              (match e
                [(application rator rands)
                 (apply set-union
                        (inner0 rator)
                        (map inner0 rands))]
                [(apply-values proc args-expr)
                 (set-union (inner0 proc)
                            (inner0 args-expr))]
                [(assign id rhs undef-ok?)
                 (inner0 rhs)]
                [(beg0 seq)
                 (apply set-union (map inner0 seq))]
                [(boxenv pos body)
                 (inner0 body)]
                [(branch test then else)
                 (set-union (inner0 test)
                            (inner0 then)
                            (inner0 else))]
                [(case-lam name clauses)
                 (apply set-union (map inner0 clauses))]
                [(closure code id)
                 (inner0 code)]
                [(compilation-top max-let-depth prefix code)
                 (inner0 code)]
                [(def-values ids rhs)
                 (inner0 rhs)]
                [(install-value count pos boxes? rhs body)
                 (set-union (inner0 rhs)
                            (inner0 body))]
                [(lam name flags num-params param-types rest? closure-map
                      closure-types toplevel-map max-let-depth body)
                 (inner0 body)]
                [(let-one rhs body type unused?)
                 (set-union (inner0 rhs)
                            (inner0 body))]
                [(let-rec procs body)
                 (set-union (apply set-union (map inner0 procs))
                            (inner0 body))]
                [(let-void count boxes? body)
                 (inner0 body)]
                [(localref unbox? pos clear? other-clears? type)
                 (seteq)]
                [(mod name srcname self-modidx prefix provides requires body
                      syntax-bodies unexported max-let-depth dummy lang-info
                      internal-context flags pre-submodules post-submodules)
                 (apply set-union (map inner0 body))]
                [(prefix num-lifts toplevels stxs)
                 (seteq)]
                [(primval id)
                 (seteq)]
                [(seq forms)
                 (apply set-union (map inner0 forms))]
                [(toplevel depth pos const? ready?)
                 (seteq)]
                [(topsyntax depth pos midpt)
                 (seteq)]
                [(varref toplevel dummy)
                 (set-union (inner0 toplevel)
                            (inner0 dummy))]
                [(with-cont-mark key val body)
                 (set-union (inner0 key)
                            (inner0 val)
                            (inner0 body))])))
        (seteq)))
  ((inner (seteq)) e))