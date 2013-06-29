#lang racket/base
(require racket/list
         racket/match
         racket/package
         compiler/zo-structs
         "primitive-maps.rkt"
         "store.rkt")

(define (primitive-id->value* id)
  (displayln (primitive-id->name id))
  (primitive-id->value id))

(define (list-set l n v)
  (cond
    [(empty? l)
     (error 'list-set "need ~a more slot(s) for ~a" n v)]
    [(zero? n)
     (cons v (rest l))]
    [else
     (cons (first l) (list-set (rest l) (sub1 n) v))]))

(define-package prefixp (empty-prefix
                         prefix-ref
                         prefix-set)
  (define (empty-prefix) (hasheq))
  (define prefix-ref hash-ref)
  (define prefix-set hash-set))

(define-package environment (env-push
                             env-ref
                             env-set
                             empty-env)
  (define empty-env empty)
  (define env-ref list-ref)
  (define env-set list-set)
  
  (define (env-push env n v)
    (if (zero? n)
        env
        (cons v (env-push env (sub1 n) v)))))


(define-package value (value?
                       single-value
                       multiple-values
                       list->value
                       value->list
                       with-single-value)
  (struct value () #:prefab)
  (struct single-value value (v) #:prefab)
  (struct multiple-values value (v) #:prefab)
  
  (define (list->value vs)
    (if (= (length vs) 1)
        (single-value (first vs))
        (multiple-values vs)))
  
  (define value->list
    (match-lambda
      [(single-value v)
       (list v)]
      [(multiple-values vs)
       vs]))
  
  (define (with-single-value f e)
  (match e
    [(single-value v)
     (f v)]
    [(multiple-values vs)
     (error 'interp "expected single value; received ~a" vs)])))

(define-package continuation (app-rands-cont
                              app-rator-cont
                              apply-values-args-cont
                              apply-values-proc-cont
                              branch-cont
                              empty-cont
                              def-values-cont
                              install-value-cont
                              let-one-cont
                              seq-cont
                              splice-cont)
  (struct cont () #:prefab)
  (struct app-rands-cont (rator index rands env cont) #:prefab)
  (struct app-rator-cont (rands env cont) #:prefab)
  (struct apply-values-args-cont cont (proc env cont) #:prefab)
  (struct apply-values-proc-cont cont (args-expr env cont) #:prefab)
  (struct branch-cont cont (then else env cont) #:prefab)
  (struct empty-cont cont () #:prefab)
  (struct def-values-cont cont (ids env cont) #:prefab)
  (struct install-value-cont cont (count pos boxes? body env cont) #:prefab)
  (struct let-one-cont cont (body env cont) #:prefab)
  (struct seq-cont cont (exprs env cont) #:prefab)
  (struct splice-cont cont (forms env cont) #:prefab))

(struct static-closure (lam) #:prefab)
(struct dynamic-closure (lam cls) #:prefab)

(require racket/format)

(define (interp zo)
  (open-package prefixp)
  (open-package environment)
  (open-package continuation)
  (open-package value)
  (define (call fun args env str kon)
    (let ([n (length args)])
      (match fun
        [(? procedure? p)
         (printf "CALL: (~a)\n" p (apply ~a p args #:separator " "))
         (if (procedure-arity-includes? p n)
             (let ([vs (call-with-values (λ () (apply p args)) list)])
               (inner (list->value vs) env str kon))
             (error 'call "~a cannot accept ~a arguments" p n))]
        [(dynamic-closure (lam _ _ m _ rest? _ _ _ _ body) cls)
         (if (or (= n m) (and rest? (> n m)))
             (let* ([env (append (take args m) env)]
                    [env (if rest? (cons (drop args m) env) env)]
                    [env (append cls env)])
               (inner body env str kon))
             (error 'call "closure with ~a param(s) (rest is ~a) cannot accept ~a argument(s)" m rest? n))]
        [(static-closure (lam _ _ m _ rest? _ _ _ _ body))
         (if (or (= n m) (and rest? (> n m)))
             (let* ([env (append (take args m) env)]
                    [env (if rest? (cons (drop args m) env) env)])
               (inner body env str kon))
             (error 'call "closure with ~a param(s) (rest? is ~a) cannot accept ~a argument(s)" m rest? n))])))
       
  (define (inner con env str kon)
   #;(when (or (value? con)
              (branch? con)
              (application? con)
              (primval? con)
              (localref? con))
      #;(displayln env)
      (printf "~a\n~a\n\n" con env))
    (if (value? con)
        (match kon
          [(app-rands-cont rator n (list) env kon)
           (with-single-value
            (λ (arg)
              (let ([env (env-set env n arg)]
                    [n (add1 n)])
                (call rator (take env n) (drop env n) str kon)))
            con)]
          [(app-rands-cont rator n (cons expr exprs) env kon)
           (with-single-value
            (λ (arg)
              (let ([env (env-set env n arg)])
                (inner expr env str (app-rands-cont rator (add1 n) exprs env kon)))) con)]
          [(app-rator-cont (list) env kon)
           (with-single-value
            (λ (fun) (call fun empty env str kon)) con)]
          [(app-rator-cont (cons expr exprs) env kon)
           (with-single-value
            (λ (v) (inner expr env str (app-rands-cont v 0 exprs env kon))) con)]
          [(apply-values-args-cont proc env kon)
           (let ([vs (value->list con)])
             (call proc vs env str kon))]
          [(apply-values-proc-cont args-expr env kon)
           (with-single-value
            (λ (v) (inner args-expr env str (apply-values-args-cont v env kon))) con)]
          [(branch-cont then else env kon)
           (with-single-value
            (λ (v) (inner (if v then else) env str kon)) con)]
          [(empty-cont)
           con]
          [(def-values-cont ids env kon)
           (let ([vs (value->list con)])
             (if (= (length ids)
                    (length vs))
                 (let ([str (foldl
                             (λ (id v str)
                               (match-let ([(toplevel depth pos const? ready?) id])
                                 (let-values ([(str addr) (store-set str v (prefix-ref (env-ref env depth) pos v))])
                                   str)))
                             str ids vs)])
                   (inner (single-value (void)) env str kon))
                 (error 'interp "expected ~a values; received ~a" (length ids) vs)))]
          [(install-value-cont count pos boxes? body env kon)
           (let ([vs (value->list con)])
             (if (= (length vs) count)
                 (if boxes?
                     (let ([str (for/fold ([str str])
                                  ([addr (take (drop env pos) count)]
                                   [v vs])
                                  (let-values ([(str addr) (store-set str v addr)])
                                    str))])
                       (inner body env str kon))
                     (let ([env (for/fold ([env env])
                                  ([i count]
                                   [v vs])
                                  (env-set env (+ pos i) v))])
                       (inner body env str kon)))
                 (error 'interp "expected ~ values; receieved ~a" count vs)))]
          [(let-one-cont body env kon)
           (with-single-value
            (λ (v)
              (let ([env (env-set env 0 v)])
                (inner body env str kon)))
            con)]
          [(seq-cont (list) env kon)
           (inner con env str kon)]
          [(seq-cont (cons expr exprs) env kon)
           (inner expr env str (seq-cont exprs env kon))]
          [(splice-cont (list) env kon)
           (inner con env str kon)]
          [(splice-cont (cons form forms) env kon)
           (inner form env str (splice-cont forms env kon))]
          [_
           (error 'inner "unhandled continuation ~a" kon)])
        (match con
          [(? boolean? p)
           (inner (single-value p) env str kon)]
          [(? number? n)
           (inner (single-value n) env str kon)]
          [(? symbol? s)
           (inner (single-value s) env str kon)]
          [(? void? v)
           (inner (single-value v) env str kon)]
          [(? list? l)
           (inner (single-value l) env str kon)]
          [(application rator rands)
           (let ([env (env-push env (length rands) (void))])
             (inner rator env str (app-rator-cont rands env kon)))]
          [(apply-values proc args-expr)
           (inner proc env str (apply-values-proc-cont args-expr env kon))]
          [(branch test then else)
           (inner test env str (branch-cont then else env kon))]
          [(case-lam name clauses)
           (inner (single-value (dynamic-closure (case-lam name clauses) env)) env str kon)]
          [(closure lam gen-id)
           (inner (single-value (static-closure lam)) env str kon)]
          [(def-values ids rhs)
           (inner rhs env str (def-values-cont ids env kon))]
          [(install-value count pos boxes? rhs body)
           (inner rhs env str (install-value-cont count pos boxes? body env kon))]
          [(and l (lam _ _ _ _ _ captures _ _ _ _))
           (inner (single-value (dynamic-closure l (map (λ (i) (env-ref env i)) (vector->list captures))))
                  env str kon)]
          [(let-one rhs body type unused?)
           (let ([env (env-push env 1 (void))])
             (inner rhs env str (let-one-cont body env kon)))]
          [(let-void count boxes? body)
           (if boxes?
               (let-values ([(env str) (for/fold ([env env]
                                                  [str str])
                                         ([i count])
                                         (let*-values ([(str addr) (store-set str (void))]
                                                       [(env) (env-push env 1 addr)])
                                           (values env str)))])
                 (inner body env str kon))
               (let ([env (env-push env count (void))])
                 (inner body env str kon)))]
               
          [(localref unbox? pos clear? other-clears? type)
           (inner (single-value (env-ref env pos)) env str kon)]
          [(primval id)
           (inner (single-value (primitive-id->value* id)) env str kon)]
          [(seq (cons expr exprs))
           (inner expr env str (seq-cont exprs env kon))]
          [(splice (cons form forms))
           (inner form env str (splice-cont forms env kon))]
          [(toplevel depth pos const? ready?)
           (inner (single-value (store-ref str (prefix-ref (env-ref env depth) pos)))
                  env str kon)]
          [_
           (displayln con)
           (displayln env)
           ;(displayln str)
           ;(displayln kon)
           (error 'inner "unhandled control ~a" con)])))
  (define (toplevel-allocate toplevels store)
    (for/fold ([pre (empty-prefix)]
               [str store])
      ([tl toplevels]
       [i (in-naturals)])
      (let*-values ([(str addr) (store-set str (void))]
                    [(pre) (if tl (prefix-set pre tl addr) pre)]
                    [(pre) (prefix-set pre i addr)])
        (values pre str))))
  (match-let ([(compilation-top _ (prefix _ top-tls _)
                                (mod _ _ _ (prefix _ mod-tls _)
                                     _ _ body _ _ _ _ _ _ _ _ _))
               zo])
      (let ([str (empty-store)])
        (let*-values ([(top-tls str) (toplevel-allocate top-tls str)]
                      [(mod-tls str) (toplevel-allocate mod-tls str)])
    (inner (splice body) (list* mod-tls top-tls empty-env) str (empty-cont))))))



(module+ main
  (require compiler/zo-parse)
  #;(with-input-from-file "../tests/scheme-to-c_rkt_merged.zo" zo-parse)
  (interp (with-input-from-file "../tests/scheme-to-c_rkt_merged.zo" zo-parse)))