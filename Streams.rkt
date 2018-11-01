#lang racket

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) ; bind f to be a function that takes one parameter
                (cons x
                      (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define nats* (stream-maker + 1))
(define powers* (stream-maker * 2))

(define ones (lambda () (cons 1 ones)))

(define nats (letrec ([f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))])
               (lambda () (f 1))))

(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
      (f stream 1)))

(define (list-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (cons (car pr) ans)))))])
      (f stream '() )))
              
(define (my-mult x y-promise)
  (cond [(= x 0) 0]
        [(= x 1) (my-force y-promise)]
        [else (+ (my-force y-promise)
                 (my-mult (- x 1) y-promise))]
        ))

(define (my-delay fun)
  (mcons #f fun))
(define (my-force th)
  (cond [(mcar th) (mcdr th)]
        [else
           (set-mcar! th #t)
           (set-mcdr! th ((mcdr th)) )
           (mcdr th)]
        ))




(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))

(define a (mcons 20 null))
(set-mcar! a 10)
(define b (mcons 30 null))
(set-mcdr! a b)

(if (< 5 3) "Hello" "GoodBye")

(define (my-if b t f) (if b (t) (f) ))

(define (factorial x)
  (my-if (= x 0)
         (lambda () 1)
         (lambda () (* x (factorial (- x 1))))))