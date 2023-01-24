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

 
(define (rco_exp e l1)
  (match e
    [(Var x) (values (Var x) l1)]
    [(Int n) (values (Int n) l1)]
    [(Prim op es)
     (Prim op (for/list ([e es]) (values (rco_exp e l1) l1)))]))
   
(define (rco_atom e l1)
  (cond
    [(atmo? e) (values e l1)]
    [else (let ([tmp (gensym)]) (values 'tmp
                                   (append (list ('tmp . (rco_exp e l1))) l1)))]))   
    ;[_ (error)]))
   ; [(Let x rhs body) (Let x (mklst))]
    ;[(Prim op (list e1 e2)) (Prim op (
                               
;; uniquify : R1 -> R1
;(define (uniquify p)
 ; (error "TODO: code goes here (uniquify)"))
(define (atmo? exp)
  (match exp
    [(Var x) #t]
    [(Int n) #t]
    [_ #f]))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program '() body) (Program '() (rco_exp body '()))]))

;; explicate-control : R1 -> C0ere (remove-complex-opera*)"))
(define (explicate-control p)
  (error))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `( 
 ; ("flip" ,flip  ,interp-Lvar ,type-check-Lvar)
    ;; Uncomment the following passes as you finish them.
     ("uniquify" ,uniquify ,interp-Lvar ,type-check-Lvar)
    ; ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar ,type-check-Lvar)
    ;; ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
    ))
