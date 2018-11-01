#lang racket

; use mutation to edit memotable as you go
(define fibmem
  (letrec ([memotable null]
           [f (lambda (x)
                (let ([ans (assoc x memotable)])
                  (cdr ans)
                  (let ([new-ans (if (or (= x 1) (= x 2))
                                     1
                                     (+ (f (= x 1))
                                        (f (= x 2))))])
                    (set! memotable (cons (cons x new-ans) memotable))
                    new-ans)))])
    f))
; also slow
(define (fib* x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))]) ; accumulating answer
    (if (or (= x 1) (= x 2))
        1
        (f 1 1 3))))

; terrible way to calculate fibonacci, because you are calculating the same thing over and over again for each step
(define (fib x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fib (- x 1))
         (fib (- x 2)))))