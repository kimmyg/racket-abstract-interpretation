#lang racket/base
(require racket/list
         racket/match
         racket/package
         [prefix-in e: "forms.rkt"])

(provide interp)

(struct closure (lam env) #:transparent)

(define empty-env empty)
(define empty-store empty)
(define empty-cont empty)


(define-package env (empty-env
                     env-extend
                     ;env-extend!
                     env-ref)
  (define (empty-env)
    (make-hasheq))
  
  (define (env-extend env params rest vs)
    (let ([env (for/fold ([env env])
                 ([x params]
                  [v (take vs (length params))])
                 (hash-set env x v))])
      (if rest
          (hash-set env rest (drop vs (length params)))
          env)))
  
  #;(define (env-extend! env params rest vs)
    (for ([x params]
          [v (take vs (length params))])
      (hash-set! env x v))
    (begin0
      env
      (when rest
        (hash-set! env rest (drop vs (length params))))))
  
  (define env-ref hash-ref))

(define-package store (empty-store store-set store-ref)
  (define (empty-store)
    (hasheq))
  
  (define store-set hash-set)
  
  (define store-ref hash-ref))

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
    [(closure (e:lambda _ _ params rest _) _)
     (if rest
         (>= n (length params))
         (= n (length params)))]))

(define primitive
  (match-lambda
    ['zero?
     zero?]
    ['*
     *]
    [x
     (error 'primitive "~a not implemented" x)]))

(define (call f vs)
  (let ([vs (call-with-values (λ () (apply f vs)) list)])
    (if (= (length vs) 1)
        (single-value (first vs))
        (multiple-values vs))))

(define (interp e)
  (open-package env)
  (open-package store)
  (open-package cont)
  (define (inner con [env (empty-env)] [str (empty-store)] [kon (empty-cont)])
    (displayln con)
    (match con
      [(? value? v)
       (match kon
         [(empty-cont)
          v]
         [(if-cont con-expr alt-expr env kon)
          (if (single-value-v v)
              (inner con-expr env str kon)
              (inner alt-expr env str kon))]
         [(module-cont (list) kon)
          (inner v env str kon)]
         [(module-cont (cons f fs) kon)
          (inner f env str (module-cont fs kon))]
         [(define-values-cont ids env kon)
          (inner (single-value (void)) (env-extend env ids #f (value->list v)) str kon)]
         [(apply-proc-cont expr env kon)
          (inner expr env str (apply-expr-cont (single-value-v v) env kon))]
         [(apply-expr-cont proc env kon)
          (let ([vs (value->list v)])
            (match proc
              [(? procedure?)
               (inner (call proc vs) env str kon)]
              [(closure (e:lambda _ _ params rest body) env)
               (inner body (env-extend env params rest vs) str kon)]))]
         [(app-fun-cont (list) env kon)
          (match v
            [(single-value (? procedure? p))
             (inner (p) env str kon)]
            [(single-value (closure (e:lambda _ _ params rest? body) env))
             (inner body env str kon)])]
         [(app-fun-cont (cons e es) env kon)
          (inner e env str (app-arg-cont (single-value-v v) empty es env kon))]
         [(app-arg-cont fun vs (cons e es) env kon)
          (inner e env str (app-arg-cont fun (cons (single-value-v v) vs) es env kon))]
         [(app-arg-cont fun vs (list) env kon)
          (let ([vs (reverse (cons (single-value-v v) vs))])
            (if (procedure-arity-includes?* fun (length vs))
                (match fun
                  [(? procedure?)
                   (inner (call fun vs) env str kon)]
                  [(closure (e:lambda _ _ params rest body) env)
                   (inner body (env-extend env params rest vs) str kon)])
                (error 'interp "incompatible function arity for ~a" vs)))]
         ;[_
          ;(error 'interp "v is ~a; no match for ~a" v kon)]
         )]
      [(? symbol? x)
       (inner (single-value (env-ref env x (λ () (primitive x)))) env str kon)]
      [(? number? n)
       (inner (single-value n) env str kon)]
      [(e:if cond-expr con-expr alt-expr)
       (inner cond-expr env str (if-cont con-expr alt-expr env kon))]
      [(e:begin (cons e es))
       (inner e env str (begin-cont es env kon))]
      [(e:module name language (cons f fs))
       (inner f env str (module-cont fs kon))]
      [(e:require modules)
       (inner (single-value (void)) env str kon)]
      [(e:define-values ids rhs)
       (inner rhs env str (define-values-cont ids env kon))]
      [(e:closed name lambda)
       (inner (single-value (closure lambda env)) env str kon)]
      [(and lambda (e:lambda _ _ _ _ _))
       (inner (single-value (closure lambda env)) env str kon)]
      [(e:apply-values procedure expression)
       (inner procedure env str (apply-proc-cont expression env kon))] 
      [(e:application function arguments)
       (inner function env str (app-fun-cont arguments env kon))]
      [e
       (error 'interp "unrecognized form ~a" e)]))
  (inner e))