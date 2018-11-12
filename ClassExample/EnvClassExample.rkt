#lang racket

(provide (all-defined-out))

;; create a function which allows name lookups in empty environment
(define (empty-env)
  (lambda (searchvar)
    (error "No Binding Found")))

(define (extend-env var val env)
  (lambda (searchvar)
    (if (equal? searchvar var)
        val ; if searchvar found, then use val
        (apply-env env searchvar)))) ; otherwise apply the environment to the searchvar

(define (apply-env env searchvar)
  (env searchvar))