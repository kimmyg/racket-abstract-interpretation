#lang racket/base

; the purpose of these structures is to provide an uncontracted
; variant of structures isomorphic to the native ones. it may be 
; possible to do this much easier with Jay's typed/liar language.

(provide (all-defined-out))

(struct zo () #:prefab)

(struct indirect zo (id) #:prefab)

(struct compilation-top zo (max-let-depth prefix code) #:prefab)
(struct prefix zo (num-lifts toplevels stxs) #:prefab)
(struct global-bucket zo (name) #:prefab)
(struct module-variable zo (modidx sym pos phase constantness) #:prefab)
(struct stx zo (encoded) #:prefab)

(struct form zo () #:prefab)

(struct def-values form (ids rhs) #:prefab)
(struct def-syntaxes form (ids rhs prefix max-let-depth dummy) #:prefab)
(struct req form (reqs dummy) #:prefab)
(struct seq form (forms) #:prefab)
(struct splice form (forms) #:prefab)
(struct inline-variant form (direct inline) #:prefab)
(struct mod form (name srcname self-modidx prefix provides requires
                  body syntax-bodies unexported max-let-depth dummy
                  lang-info internal-context flags pre-submodules
                  post-submodules) #:prefab)

(struct expr form () #:prefab)

(struct lam expr (name flags num-params param-types rest?
                  closure-map closure-types toplevel-map
                  max-let-depth body) #:prefab)
(struct closure expr (code gen-id) #:prefab)
(struct case-lam expr (name clauses) #:prefab)
(struct let-one expr (rhs body type unused?) #:prefab)
(struct let-void expr (count boxes? body) #:prefab)
(struct install-value expr (count pos boxes? rhs body) #:prefab)
(struct let-rec expr (procs body) #:prefab)
(struct boxenv expr (pos body) #:prefab)
(struct localref expr (unbox? pos clear? other-clears? type) #:prefab)
(struct toplevel expr (depth pos const? ready?) #:prefab)
(struct topsyntax expr (depth pos midpt) #:prefab)
(struct application expr (rator rands) #:prefab)
(struct branch expr (test then else) #:prefab)
(struct with-cont-mark expr (key val body) #:prefab)
(struct beg0 expr (seq) #:prefab)
(struct varref expr (toplevel dummy) #:prefab)
(struct assign expr (id rhs undef-ok?) #:prefab)
(struct apply-values expr (proc args-expr) #:prefab)
(struct primval expr (id) #:prefab)
