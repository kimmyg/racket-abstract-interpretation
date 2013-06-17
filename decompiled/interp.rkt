#lang racket/base
(require racket/list
         racket/match
         racket/package
         [prefix-in e: "forms.rkt"]
         "dynamic-closure.rkt"
         "static-closure.rkt"
         "env.rkt"
         "store.rkt")

(provide interp)

(define-package cont (empty-cont
                      if-cont
                      begin-cont
                      module-cont
                      define-values-cont
                      apply-proc-cont
                      apply-expr-cont
                      app-fun-cont
                      app-arg-cont)
  (struct cont () #:transparent)
  (struct empty-cont cont () #:transparent)
  (struct if-cont cont (con-expr alt-expr env cont) #:transparent)
  (struct begin-cont cont (es env cont) #:transparent)
  (struct module-cont cont (fs cont) #:transparent)
  (struct define-values-cont cont (ids env cont) #:transparent)
  (struct apply-proc-cont cont (expr env cont) #:transparent)
  (struct apply-expr-cont cont (proc env cont) #:transparent)
  (struct app-fun-cont cont (args env cont) #:transparent)
  (struct app-arg-cont cont (fun vs es env cont) #:transparent))


(struct value () #:transparent)
(struct single-value value (v) #:transparent)
(struct multiple-values value (vs) #:transparent)

(define value->list
  (match-lambda
    [(single-value v)
     (list v)]
    [(multiple-values vs)
     vs]))

(define (procedure-arity-includes?* p n)
  (match p
    [(? procedure?)
     (procedure-arity-includes? p n)]
    [(or (static-closure (e:lambda _ _ params rest _))
         (dynamic-closure (e:lambda _ _ params rest _) _))
     (or (= n (length params))
         (and rest
              (> n (length params))))]
    [v
     (error 'procedure-arity-includes?* "expected procedure; got ~a" v)]))

(define (call f vs)
  (let ([vs (call-with-values (λ () (apply f vs)) list)])
    (if (= (length vs) 1)
        (single-value (first vs))
        (multiple-values vs))))

(define (env/store-extend env str params rest vs)
  (let-values ([(env str)
                (for/fold ([env env]
                           [str str])
                  ([x params]
                   [v vs])
                  (let*-values ([(str addr) (store-set str v)]
                                [(env) (env-set env x addr)])
                    (values env str)))])
    (if rest
        (let*-values ([(str addr) (store-set str (drop vs (length params)))]
                      [(env) (env-set env rest addr)])
          (values env str))
        (values env str))))

; closed forms are "global" references
; see the documentation on core forms
; and the output of capturing-inner-defines.rkt

(define (interp con env str)
  (define base-env env)
  (open-package cont)
  (define (inner con env str kon)
    ;(displayln con)
    ;(displayln env)
    ;(displayln str)
    (match con
      [(? value? v)
       (match kon
         [(app-arg-cont fun vs (cons e es) env kon)
          (inner e env str (app-arg-cont fun (cons (single-value-v v) vs) es env kon))]
         [(app-arg-cont fun vs (list) env kon)
          (let ([vs (reverse (cons (single-value-v v) vs))])
            (if (procedure-arity-includes?* fun (length vs))
                (match fun
                  [(? procedure?)
                   (inner (call fun vs) env str kon)]
                  [(dynamic-closure (e:lambda _ _ params rest body) env)
                   (let-values ([(env str) (env/store-extend env str params rest vs)])
                     (inner body env str kon))]
                  [(static-closure (e:lambda _ _ params rest body))
                   (let-values ([(env str) (env/store-extend env str params rest vs)])
                     (inner body env str kon))])
                (error 'interp "incompatible function arity applying ~a to ~a" fun vs)))]
         [(app-fun-cont (list) env kon)
          (match v
            [(single-value fun)
             (if (procedure-arity-includes?* fun 0)
                 (match fun
                  [(? procedure?)
                   (inner (call fun empty) env str kon)]
                  [(dynamic-closure (e:lambda _ _ params rest body) env)
                   (inner body env str kon)]
                  [(static-closure (e:lambda _ _ params rest body))
                   (inner body base-env str kon)])
                 (error 'interp "incompatible function arity applying ~a to no arguments" fun))]
            [(multiple-values vs)
             (error 'interp "expected single function; got ~a" vs)])]
         [(app-fun-cont (cons e es) env kon)
          (inner e env str (app-arg-cont (single-value-v v) empty es env kon))]
         [(apply-proc-cont expr env kon)
          (inner expr env str (apply-expr-cont (single-value-v v) env kon))]
         [(apply-expr-cont proc env kon)
          (let ([vs (value->list v)])
            (match proc
              [(? procedure?)
               (inner (call proc vs) env str kon)]
              [(dynamic-closure (e:lambda _ _ params rest body) env)
               (let-values ([(env str) (env/store-extend env str params rest vs)])
                 (inner body env str kon))]
              [(static-closure (e:lambda _ _ params rest body))
               (let-values ([(env str) (env/store-extend base-env str params rest vs)])
                 (inner body env str kon))]))]
         [(begin-cont (list) env kon)
          (inner v env str kon)]
         [(begin-cont (cons e es) env kon)
          (inner e env str (begin-cont es env kon))]
         [(define-values-cont ids env kon)
          (let-values ([(env str) (env/store-extend env str ids #f (value->list v))])
            (inner (single-value (void)) env str kon))]
         [(empty-cont)
          v]
         [(if-cont con-expr alt-expr env kon)
          (match v
            [(single-value v)
             (if v
                 (inner con-expr env str kon)
                 (inner alt-expr env str kon))]
            [(multiple-values vs)
             (error 'interp "expected single value for test; got ~a" vs)])]
         [(module-cont (list) kon)
          (inner v env str kon)]
         [(module-cont (cons f fs) kon)
          (inner f env str (module-cont fs kon))]
         [_
         (error 'interp "v is ~a; no match for ~a" v kon)]
         )]
      [(e:application function arguments)
       (inner function env str (app-fun-cont arguments env kon))]
      [(e:apply-values procedure expression)
       (inner procedure env str (apply-proc-cont expression env kon))]
      [(e:begin (cons e es))
       (inner e env str (begin-cont es env kon))]
      [(e:define-values ids rhs)
       (inner rhs env str (define-values-cont ids env kon))]
      [(e:if cond-expr con-expr alt-expr)
       (inner cond-expr env str (if-cont con-expr alt-expr env kon))]
      [(and lambda (e:lambda _ _ _ _ _))
       (inner (single-value (dynamic-closure lambda env)) env str kon)]
      [(e:module name language (cons f fs))
       (inner f env str (module-cont fs kon))]
      [(e:quote v)
       (inner (single-value v) env str kon)]
      [(e:ref x)
       (inner (single-value (store-ref str (env-ref env x))) env str kon)]
      [(e:sfs-clear e)
       (inner e env str kon)]
      [(e:require modules)
       (inner (single-value (void)) env str kon)]
      [e
       (error 'interp "unrecognized form ~a" e)]))
  (inner con env str (empty-cont)))