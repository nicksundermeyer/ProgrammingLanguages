#lang racket

(require "AbstractSyntaxTreeClassExample.rkt"
         "ParserClassExample.rkt"
         "EnvClassExample.rkt")
(provide (all-defined-out))

;; evaluation function
(define (eval expression)
  (evalHelper expression (empty-env)))

;; evaluation helper function
(define (evalHelper expr env)
  (match expr
    [(plus-expr e1 e2) (+ (evalHelper e1 env) (evalHelper e2 env))]
    [(let-expr name val e) (evalHelper e (extend-env (identifier-expr-argone name) (evalHelper val env) env))]
    ; find def of funname in env
    ; evaluate parameters by adding a new binding formal parameter to the actual parameter value, then evaluate function in new environment
    ; use closure to bundle together function with everything you need to know to evaluate it at the time it is created
    [(function-app funname parameter)
     (let ([fundef (apply-env env funname)]) ; get function def from environment
          (evalHelper (closure-body fundef)
                      (extend-env (closure-parameter fundef) (evalHelper parameter env) (closure-env fundef))))] ; evaluate actual parameter before binding, extracting paremeter out of funname
    [(lambda-expr parameter body) (closure parameter body env)]
    [(numeric-expr e) e]
    [(boolean-expr e) e]
    [(identifier-expr e) (apply-env env e)]
    ))

; (eval (parsestr "(let (x 5) (plus x 3))")) == 8
; (eval (parsestr "(let (f (lambda (x) (plus x 2))) (f 3))")) == 5