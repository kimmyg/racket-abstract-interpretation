#lang racket/base
(require racket/format
         racket/list
         racket/match
         racket/package
         racket/set
         compiler/zo-parse
         compiler/zo-structs
         "../../primitive-maps.rkt")

(define-package pkg:prefix (store:prefix
                            prefix-ref)
  (struct store:prefix (toplevels) #:transparent)
  
  (define (prefix-ref prefix pos)
    (vector-ref (store:prefix-toplevels prefix) pos)))
                            
(open-package pkg:prefix)

(define-package env (empty-env
                     env-push
                     env-ref
                     env-set
                     env-take
                     env-drop)
  
  (define empty-env empty)
  
  (define (env-push env . vs)
    (append vs env))
  
  (define (env-ref env n)
    (list-ref env n))
  
  (define (env-set env n v)
    (if (zero? n)
        (cons v (rest env))
        (cons (first env) (env-set (rest env) (sub1 n) v))))
  
  (define env-take take)
  (define env-drop drop))

(open-package env)

(define fresh
  (let ([counts (make-hasheq)])
    (λ (sym)
      (begin0
        (string->symbol (~a sym (hash-ref counts sym 0)))
        (hash-update! counts sym add1 0)))))

(define-package store (empty-store
                       store-ref
                       store-set)
  (define empty-store (hasheq))
  (define store-ref hash-ref)
  (define store-set
    (case-lambda
      [(str v)
       (let ([addr (fresh 'addr)])
         (values (store-set str addr v) addr))]
      [(str addr v)
       #;(when (eq? addr 'addr2)
         (error 'addr))
       (when (void? addr)
         (error 'void))
       (hash-set str addr v)])))

(open-package store)

(define-package continuation (empty-cont
                              env-cont
                              application-rands-cont
                              application-rator-cont
                              apply-values-args-cont
                              apply-values-proc-cont
                              branch-cont
                              def-values-cont
                              seq-cont
                              splice-cont)
  (struct cont () #:transparent)
  
  (struct empty-cont cont () #:transparent)
  
  (struct env-cont cont (cont env) #:transparent)
  
  (struct application-rator-cont env-cont (rands) #:transparent)
  (struct application-rands-cont env-cont (rator n es) #:transparent)
  (struct apply-values-args-cont env-cont (proc) #:transparent)
  (struct apply-values-proc-cont env-cont (args-expr) #:transparent)
  (struct branch-cont env-cont (then else) #:transparent)
  (struct def-values-cont env-cont (ids) #:transparent)
  (struct seq-cont env-cont (forms) #:transparent)
  (struct splice-cont env-cont (forms) #:transparent))

(open-package continuation)

(define-package value (value?
                       single-value
                       multiple-values
                       list->value
                       value->list)
  (struct value () #:transparent)
  (struct single-value value (v) #:transparent)
  (struct multiple-values value (vs) #:transparent)
  
  (define value->list
    (match-lambda
      [(single-value v)
       (list v)]
      [(multiple-values vs)
       vs]))
  
  (define (list->value vs)
    (if (= (length vs) 1)
        (single-value (first vs))
        (multiple-values vs))))

(open-package value)

(struct clos () #:transparent)
(struct static-clos (lam) #:transparent)
(struct dynamic-clos (lam env) #:transparent)

(define (read path)
  (with-input-from-file path zo-parse))

(struct state () #:transparent)
(struct CESK state (control base-environment environment store continuation) #:transparent)
(struct error* state (message) #:transparent)

(define (env/store-set env str n v)
  (let ([str (store-set str (env-ref env n) v)])
    (values env str)))

(define (env/store-ref env str n)
  (store-ref str (env-ref env n)))

(define (env/store-push env str . vs)
  (for/fold ([env env]
             [str str])
    ([v (reverse vs)])
    (let*-values ([(str addr) (store-set str v)]
                  [(env) (env-push env addr)])
      (values env str))))

(define (env/store-prefix env str prefix*)
  (match-let ([(prefix num-lifts toplevels stxs) prefix*])
    (let*-values ([(toplevels) (map (λ (toplevel) (or toplevel (fresh 'toplevel))) toplevels)]
                  [(str) (for/fold ([str str])
                           ([toplevel toplevels])
                           (store-set str toplevel (void)))]
                  [(str addr) (store-set str (store:prefix (list->vector toplevels)))]
                  [(env) (env-push env addr)])
      (values env str))))

(define (inject zo)
  (match-let* ([(compilation-top max-let-depth prefix* code) zo]
               [(mod name srcname self-modidx prefix provides requires body
                     syntax-bodies unexported max-let-depth dummy lang-info
                     internal-context flags pre-submodules post-submodules)
                code])
    (let*-values ([(env str) (values empty-env empty-store)]
                  [(env str) (env/store-prefix env str prefix*)]
                  [(env str) (env/store-prefix env str prefix)])
      (CESK (splice body) env env str (empty-cont)))))

(define (procedure-arity-includes?* n rest? m)
  (or (= m n)
      (and rest? (> m n))))

(define (call proc args base env str kon)
  (match proc
    [(? procedure?)
     (if (procedure-arity-includes? proc (length args))
         (let* ([args (map (λ (addr) (store-ref str addr)) args)]
                [v (list->value (call-with-values (λ () (apply proc args)) list))])
           (CESK v base env str kon))
         (error* "arity mismatch"))]
    [(dynamic-clos (lam name flags num-params param-types rest? closure-map
                       closure-types toplevel-map max-let-depth body) closure)
     (if (procedure-arity-includes?* num-params rest? (length args))
         (let*-values ([(env str) (values base str)]
                       [(env str) (if rest? (env/store-push env str (map (λ (addr) (store-ref str addr)) (drop args num-params))) (values env str))]
                       [(env) (apply env-push env (take args num-params))]
                       [(env) (apply env-push env closure)])
           (CESK body base env str kon))
         (error* "arity mismatch"))]
    [(static-clos (lam name flags num-params param-types rest? closure-map
                       closure-types toplevel-map max-let-depth body))
     
     (if (procedure-arity-includes?* num-params rest? (length args))
         (let*-values ([(env str) (values base str)]
                       [(env str) (if rest? (env/store-push env str (map (λ (addr) (store-ref str addr)) (drop args num-params))) (values env str))]
                       [(env) (apply env-push env (take args num-params))])
           (CESK body base env str kon))
         (error* "arity mismatch"))]))
                       

(define step
  (match-lambda
    [(CESK con base env str kon)
     (match con
       [(? value? v)
        (match kon
          [(application-rands-cont kon env proc i (list))
           (match v
             [(single-value v)
              (let-values ([(env str) (env/store-set env str i v)])
                (let ([args (env-take env (add1 i))]
                      [env (env-drop env (add1 i))])
                  (call proc args base env str kon)))]
             [(multiple-values vs)
              (error* (format "received multiple values for rand ~a" vs))])]
          [(application-rands-cont kon env proc i (cons e es))
           (match v
             [(single-value v)
              (let-values ([(env str) (env/store-set env str i v)])
                (CESK e base env str (application-rands-cont kon env proc (add1 i) es)))]
             [(multiple-values vs)
              (error* (format "received multiple values for rand ~a" vs))])]
          [(application-rator-cont kon env (cons e es))
           (match v
             [(single-value v)
              (CESK e base env str (application-rands-cont kon env v 0 es))]
             [(multiple-values vs)
              (error* (format "received multiple values for rator ~a" vs))])]
          [(application-rator-cont kon env (list))
           (match v
             [(single-value v)
              (call v empty base env str kon)]
             [(multiple-values vs)
              (error* (format "received multiple values for rator ~a" vs))])]
          [(apply-values-args-cont kon env proc)
           (match v
             [(single-value v)
              (let-values ([(str args) (let-values ([(str addr) (store-set str v)])
                                         (values str (list addr)))])
                (call proc args base env str kon))]
             [(multiple-values vs)
              (let-values ([(str args) (for/fold ([str str]
                                                  [addrs empty])
                                         ([v (reverse vs)])
                                         (let-values ([(str addr) (store-set str v)])
                                           (values str (cons addr addrs))))])
                (call proc args base env str kon))])]
          [(apply-values-proc-cont kon env args-expr)
           (match v
             [(single-value v)
              (CESK args-expr base env str (apply-values-args-cont kon env v))]
             [(multiple-values vs)
              (error* (format "received multiple values for proc ~a" vs))])]
          [(branch-cont kon env then else)
           (match v
             [(single-value v)
              (CESK (if v then else) base env str kon)]
             [(multiple-values vs)
              (error* (format "received multiple values for branch ~a" vs))])]
          [(def-values-cont kon env ids)
           (let ([vs (value->list v)])
             (if (= (length ids)
                    (length vs))
                 (let ([str (foldl (λ (id v str)
                                     (match-let ([(toplevel depth pos const? ready?) id])
                                       (store-set str (prefix-ref (env/store-ref env str depth) pos) v)))
                                   str ids vs)])
                   (CESK (single-value (void)) base env str kon))
                 (error* "def-values arity mismatch")))]
          [(empty-cont)
           v]
          [(seq-cont kon env (cons form forms))
           (CESK form base env str (seq-cont kon env forms))]
          [(seq-cont kon env (list))
           (CESK v base env str kon)]
          [(splice-cont kon env (cons form forms))
           (CESK form base env str (splice-cont kon env forms))]
          [(splice-cont kon env (list))
           (CESK v base env str kon)]
          [_
           (error 'continuation "~a" kon)])]
       [(? number? n)
        (CESK (single-value n) base env str kon)]
       [(application rator rands)
        (let-values ([(env str) (for/fold ([env env]
                                           [str str])
                                  ([rand rands])
                                  (env/store-push env str (void)))])
          (CESK rator base env str (application-rator-cont kon env rands)))]
       [(apply-values proc-expr args-expr)
        (CESK proc-expr base env str (apply-values-proc-cont kon env args-expr))]
       [(branch test then else)
        (CESK test base env str (branch-cont kon env then else))]
       [(closure code gen-id)
        (CESK (single-value (static-clos code)) base env str kon)]
       [(def-values ids rhs)
        (CESK rhs base env str (def-values-cont kon env ids))]
       [(and lam* (lam name flags num-params param-types rest? closure-map
                       closure-types toplevel-map max-let-depth body))
        (let ([closure (map (λ (n) (env-ref env n)) (vector->list closure-map))])
          (CESK (single-value (dynamic-clos lam* closure)) base env str kon))]
       [(localref unbox? pos clear? other-clears? type)
        (let ([v (env/store-ref env str pos)]
              [env (if clear? (env-set env pos 'undefined) env)])
          (CESK (single-value v) base env str kon))]
       [(primval id)
        ;(displayln (primitive-id->name id))
        (CESK (single-value (primitive-id->value id)) base env str kon)]
       [(seq (cons form forms))
        (CESK form base env str (seq-cont kon env forms))]
       [(splice (cons form forms))
        (CESK form base env str (splice-cont kon env forms))]
       [(toplevel depth pos const? ready?)
        (CESK (single-value (store-ref str (prefix-ref (env/store-ref env str depth) pos))) base env str kon)]
       [_
        (error 'control "~a" con)])]))

(define (done? σ)
  (or (error*? σ)
      (value? σ)))


(define-package garbage-collection (close
                                    current-store
                                    direct-object-references)
  (define current-store (make-parameter (hasheq)))
  
  (define (close addrs)
    (close* (filter-not (λ (addr) (eq? addr 'undefined)) addrs) (seteq)))
  
  (define (close* addrs references)
    (foldl add-address references addrs))
  
  (define (add-address addr references)
    (if (set-member? references addr)
        references
        (close* (direct-object-references (store-ref (current-store) addr)) (set-add references addr))))
  
  (define direct-object-references
    (match-lambda
      [(single-value v)
       (direct-object-references v)]
      [(multiple-values vs)
       (apply append (map direct-object-references vs))]
      [(dynamic-clos lambda closure)
       closure]
      [(store:prefix toplevels)
       (vector->list toplevels)]
      [(env-cont kon env)
       (append env (direct-object-references kon))]
      [_
       empty])))

(open-package garbage-collection)

(define gc
  (match-lambda
    [(CESK con base env str kon)
     ;(displayln str)
     (let* ([live-addrs (parameterize ([current-store str])
                         (set-union (close (direct-object-references con))
                                    (close env)
                                    (close (direct-object-references kon))))]
            [str* (for/fold ([str* empty-store])
                    ([addr live-addrs])
                    (store-set str* addr (store-ref str addr)))])
       (let ([dumped (set-subtract (apply seteq (hash-keys str)) live-addrs)])
         (unless (zero? (set-count dumped))
           (displayln dumped)))
       (CESK con base env str* kon))]
    [x
     x]))

(define display-state
  (match-lambda
    [(CESK con base env str kon)
     (displayln con)
     (displayln base)
     (displayln env)
     (displayln str)
     (displayln kon)]
    [(single-value v)
     (displayln v)]))

(define (eval σ)
  (if (done? σ)
      σ
      (eval (step (gc σ)))))


(eval (inject (read "../../../tests/fact_rkt_merged.zo")))