#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(define (flip p)
  (define (flip-e e)
    (match e
      [(Int n) (Int n)]
      [(Var x) (Var x)]
      [(Prim op (list e1 e2)) (Prim op (list (flip-e e2) (flip-e e1)))]
      [(Prim op es) (Prim op (for/list [(e es)] (flip-e e)))]
      [(Let x rhs body) (Let x (flip-e rhs) (flip-e body))]
      [_ (error "Nothing matches")]))
   (match p
    [(Program info body)
     (Program info (flip-e body))]))


(define (uniquify_exp env)
  (lambda (e)
  (match e
    [(Var x) (Var (dict-ref env 'x))]
    [(Int n) (Int n)]
    [(Let x e body) (let ([env (dict-set env 'x (gensym))])
                       (Let (dict-ref env 'x) ((uniquify_exp env) e) ((uniquify_exp env) body)))]
    [(Prim op es)
     (Prim op (for/list ([e es]) ((uniquify_exp env) e)))])))

(define (uniquify p)
  (match p
    [(Program '() e) (Program '() ((uniquify_exp '()) e))]))




;; uniquify : R1 -> R1
;(define (uniquify p)
 ; (error "TODO: code goes here (uniquify)"))
(define (atmo? exp)
  (match exp
    [(Var x) #t]
    [(Int n) #t]
    [_ #f]))

(define (simple? e)
  (match e  
  [(Prim op args) (andmap atmo? args)]
  [(Let x e1 e2) (and (simple? e1) (simple? e2))]
  [_(atmo? e)]))

(define (rco e lst)
  (error))

(define (rco_atom e lst)
  (cond
    [(atmo? e) (values e lst)]
    [(simple? e) (match e
                   [(Prim op args) (let ([t1 (gensym)])
                                     (values (Var t1) (dict-set lst t1 (Prim op args))))]
                   ;terminal
                   [(Let x e1 e2) (begin
                                    (set! lst (dict-set lst x e1))
                                    (let-values ([(e_out l_out) (rco_atom e2 lst)])
                                      (values e_out l_out)))])]
    [else
     (match e
       [(Prim op (list e1 e2));non-terminal
                                 (let-values ([(e1_out l1_out) (rco_atom e1 lst)])
                                   (let-values ([(e2_out l2_out) (rco_atom e2 lst)])
                                     (begin
                                       (set! lst (remove-duplicates (append l1_out l2_out)))
                                       (rco_atom (Prim op (list e1_out e2_out)) lst))))]
       [(Prim op (list e1)) (let-values ([(e_out l_out) (rco_atom e1 lst)])
                              (rco_atom (Prim op (list e_out)) l_out))]
       [(Let x e1 e2) (Let x (rco_exp e1) (rco_exp e2))])]))

(define (rco_exp e)
  (if (simple? e) e
      (match e
        [(Prim op (list e1 e2)) (let-values ([(e1_out l1_out) (rco_atom e1 '())]) ;;;;TERMINAL 
                                  (let-values ([(e2_out l2_out) (rco_atom e2 '())])
                                    (gen-code (remove-duplicates (append l1_out l2_out)) (Prim op (list e1_out e2_out)))))]
        [(Prim op (list e1)) (let-values ([(atm l_out) (rco_atom e1 '())])
                              (gen-code l_out (Prim op (list atm))))]
        [(Let x e1 e2) (Let x (rco_exp e1) (rco_exp e2))])))

(define (gen-code lst e)
  (match lst
    ['() e]
    [(cons pair others) (match pair
                          [(cons var eqn) (Let var eqn (gen-code others e))])]))

;Integers and Variables 29
(define (explicate_tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body) (explicate_assign rhs x (explicate_tail body))]
    [(Prim op es) (Return (Prim op es))]
    [else (error "explicate_tail unhandled case" e)]))


(define (explicate_assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Let y rhs body) (explicate_assign rhs y (explicate_assign body x cont))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]

    [else (error "explicate_assign unhandled case" e)]))

(define (list-vars tail lst)
  (match tail
    [(Return something) lst]
    [(Seq (Assign (Var x) exp) next) (list-vars next (cons x lst))]))



(define (explicate-control p)
  (match p
    [(Program info body) (let ([tail (explicate_tail body)])
                           (let ([info_out (list-vars tail '())])
                             (CProgram (dict-set '() 'locals (list info_out)) (dict-set '() 'start tail))))]))


   
;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program '() e) (Program '() (rco_exp e))]))

;; explicate-control : R1 -> C0ere (remove-complex-opera*)"))
;(define (explicate-control p)
 ; (error))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `( 
 ; ("flip" ,flip  ,interp-Lvar ,type-check-Lvar)
    ;; Uncomment the following passes as you finish them.
     ("uniquify" ,uniquify ,interp-Lvar ,type-check-Lvar)
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar ,type-check-Lvar)
     ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
    ))
