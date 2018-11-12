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
    [(numeric-expr e) e]
    [(boolean-expr e) e]
    [(identifier-expr e) (apply-env env e)]
    ))

; (eval (parsestr "(let (x 5) (plus x 3))")) == 8