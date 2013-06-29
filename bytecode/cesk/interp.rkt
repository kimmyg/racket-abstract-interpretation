#lang racket/base
(require racket/list
         racket/match
         racket/package
         "env.rkt"
         "store.rkt"
         "interpreter-structs.rkt"
         "dynamic-closure.rkt"
         "static-closure.rkt")

(provide interp)

(define-package continuation (application-args-cont
                              application-fun-cont
                              apply-values-proc-cont
                              apply-values-args-cont
                              branch-cont
                              def-values-cont
                              empty-cont
                              seq-cont
                              splice-cont)
  (struct cont () #:transparent)
  
  (struct application-args-cont cont (fun vs args env cont) #:transparent)
  (struct application-fun-cont cont (args env cont) #:transparent)
  (struct apply-values-proc-cont cont (args-expr env cont) #:transparent)
  (struct apply-values-args-cont cont (proc env cont) #:transparent)
  (struct branch-cont (then else env cont) #:transparent)
  (struct def-values-cont cont (ids env cont) #:transparent)
  (struct empty-cont cont () #:transparent)
  (struct seq-cont cont (es env cont) #:transparent)
  (struct splice-cont cont (fs env cont) #:transparent))

(define-package value (value?
                       single-value
                       multiple-values
                       value->list
                       with-single-value)
  (struct value () #:transparent)
  
  (struct single-value value (v) #:transparent)
  (struct multiple-values value (vs) #:transparent)
  
  (define value->list
    (match-lambda
      [(single-value v)
       (list v)]
      [(multiple-values vs)
       vs]))
  
  (define (with-single-value f v)
    (match v
      [(single-value v)
       (f v)]
      [(multiple-values vs)
       (error 'interp "expected single value; received ~a" vs)])))

(define primitive-lookup
  (match-lambda
    ['*
     *]
    ['apply
     apply]
    ['current-print
     current-print]
    ['for-each
     for-each]
    ['sub1
     sub1]
    ['values
     values]
    ['zero?
     zero?]
    [x
     (error 'primitive "not handling ~a" x)]))

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

; a compilation-top has a prefix of global variable slots
; a module has a prefix of module global variables
; procedure constants should be considered module global also

; for our purposes, the global and module prefix are the same
; 



(define (interp con env str) ; base environment
  (open-package continuation)
  (open-package value)
  (define (call fun args)
    (let ([vs (call-with-values (λ () (apply fun args)) list)])
      (if (= (length vs) 1)
          (single-value (first vs))
          (multiple-values vs))))
  (define (fun-apply fun args env str kon)
    (match fun
      [(? procedure?)
       (inner (call fun args) env str kon)]
      [(static-closure (lam params rest body))
       (let-values ([(env str) (env/store-extend env str params rest args)])
         (inner body env str kon))]
      [(dynamic-closure (lam params rest body) env)
       (let-values ([(env str) (env/store-extend env str params rest args)])
         (inner body env str kon))]
      [_
       (error 'interp "expected a procedure; received ~a" fun)]))
  (define (inner con env str kon)
    (match con
      [(? value? v)
       (match kon
         [(application-args-cont fun vs (list) env kon)
          (with-single-value
           (λ (v)
             (let ([vs (reverse (cons v vs))])
               (fun-apply fun vs env str kon)))
           v)]
         [(application-args-cont fun vs (cons e es) env kon)
          (with-single-value
           (λ (v)
             (inner e env str (application-args-cont fun (cons v vs) es env kon)))
           v)]
         [(application-fun-cont (list) env kon)
          (with-single-value
           (λ (fun)
             (fun-apply fun empty env str kon))
           v)]
         [(application-fun-cont (cons e es) env kon)
          (with-single-value
           (λ (fun)
             (inner e env str (application-args-cont fun empty es env kon)))
           v)]
         [(apply-values-args-cont fun env kon)
          (fun-apply fun (value->list v) env str kon)]
         [(apply-values-proc-cont args-expr env kon)
          (with-single-value
           (λ (proc)
             (inner args-expr env str (apply-values-args-cont proc env kon)))
           v)]
         [(branch-cont then else env kon)
          (with-single-value
           (λ (p)
             (inner (if p then else) env str kon))
           v)]   
         [(def-values-cont ids env kon)
          (let ([vs (value->list v)])
            (if (= (length ids)
                   (length vs))
                (let ([str (foldl
                            (λ (tl v str)
                              (let-values ([(str addr) (store-set str v (env-ref env (toplevel-id tl)))])
                                str))
                            str ids vs)])
                  (inner (single-value (void)) env str kon))
                (error 'interp "expected ~a values; received ~a" (length ids) vs)))]
         [(empty-cont)
          v]
         [(seq-cont (list) env kon)
          (inner v env str kon)]
         [(seq-cont (cons e es) env kon)
          (inner e env str (seq-cont es env kon))]
         [(splice-cont (list) env kon)
          (inner v env str kon)]
         [(splice-cont (cons f fs) env kon)
          (inner f env str (splice-cont fs env kon))]
         [_
          (error 'interp "unhandled continuation type: ~a" kon)])]
      [(? boolean? p)
       (inner (single-value p) env str kon)]
      [(? number? n)
       (inner (single-value n) env str kon)]
      [(application fun args)
       (inner fun env str (application-fun-cont args env kon))]
      [(apply-values proc args-expr)
       (inner proc env str (apply-values-proc-cont args-expr env kon))]
      [(branch test then else)
       (inner test env str (branch-cont then else env kon))]
      [(def-values ids rhs)
       (inner rhs env str (def-values-cont ids env kon))]
      [(and lambda (lam params rest body))
       (inner (single-value (dynamic-closure lambda env)) env str kon)]
      [(localref id)
       (inner (single-value (store-ref str (env-ref env id))) env str kon)]
      [(primval id)
       (inner (single-value (primitive-lookup id)) env str kon)]
      [(seq (cons e es))
       (inner e env str (seq-cont es env kon))]
      [(splice (cons f fs))
       (inner f env str (splice-cont fs env kon))]
      [(static-ref id)
       (inner (single-value (store-ref str (env-ref env id))) env str kon)]
      [(toplevel id)
       (inner (single-value (store-ref str (env-ref env id))) env str kon)]
      [_
       (error 'interp "unhandled control type: ~a" con)]))
  (inner con env str (empty-cont)))

#;(define run
    (match-lambda
      [(? boolean?)
       (void)]
      [(? list? xs)
       (for-each run xs)]
      [(? module-path-index?)
       (void)]
      [(? number?)
       (void)]
      [(? path?)
       (void)]
      [(? symbol?)
       (void)]
      [(? vector? xs)
       (for ([x xs])
         (run x))]
      [(application rator rands)
       (run rator)
       (run rands)]
      [(apply-values proc args-expr)
       (run proc)
       (run args-expr)]
      [(branch test then else)
       (run test)
       (run then)
       (run else)]
      [(closure code gen-id)
       (run code)
       (run gen-id)]
      [(def-values ids rhs)
       (run ids)
       (run rhs)]
      [(lam name flags num-params param-types rest? closure-map
            closure-types toplevel-map max-let-depth body)
       (run name)
       (run flags)
       (run num-params)
       (run param-types)
       (run rest?)
       (run closure-map)
       (run closure-types)
       (run toplevel-map)
       (run max-let-depth)
       (run body)]
      [(localref unbox? pos clear? other-clears? type)
       (run unbox?)
       (run pos)
       (run clear?)
       (run other-clears?)
       (run type)]
      [(prefix num-lifts toplevels stxs)
       (run num-lifts)
       (run toplevels)
       (run stxs)]
      [(primval id)
       (run id)]
      [(seq forms)
       (run forms)]
      [(toplevel depth pos const? ready?)
       (run depth)
       (run pos)
       (run const?)
       (run ready?)]
      [f
       (error 'run "~a" f)]))