#lang racket

(define (sumList xs)
    (if (null? xs)
        0
        (if (number? (first xs))
            (+ (first xs) (sumList(rest xs)))
            (+ (sumList(first xs)) (sumList(rest xs))))))

(define fact
    (lambda (n)
        (if (= n 0)
            1
            (n * (fact (- n 1))))))

(define (cube n)
    (* n (* n n)))

(define (cube2 n)
    (* n n n))

(define (fact2 n) (if (= n 0) 1 (* n (fact2 (= (- n 1))))))

(define (fact3 n)
    (if (= n 0)
        (1)
        (* n (fact3 (- n 1)))))

(define foo
  (lambda () (1 2)))