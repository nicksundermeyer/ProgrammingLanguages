#lang racket

(require 2htdp/planetcute)
(require test-engine/racket-tests)

; natural numbers, for testing
(define nats (letrec ((f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))))
               (lambda () (f 1))))

(define (downseries step high low)
  (cond
    [(< high low) empty]
    [else (cons high (downseries step (- high step) low))]))

(define (meow-string-map lst)
  (map (lambda (string) (string-append string "meow")) lst))

(define (list-ref-div lst n)
  (cond
    [(null? lst) (error "list-ref-div: the list is empty")]
    [(< n 0) (error "list-ref-div: negative number")]
    [else (list-ref lst (quotient n (length lst)))]))

(define (next-k-items s k)
  (letrec ([f (lambda (s k ans)
                (let ([pr (s)])
                  (if (<= k 0)
                      (reverse ans)
                      (f (cdr pr) (- k 1) (cons (car pr) ans)))))])
    (f s k '() )))

; Find kth item by finding next k items, taking last one
(define (kth-item s k)
  (list-ref (next-k-items s k) (- k 1)))

(define negate-2-and-5
  (letrec ((f (lambda (x)
                (cond
                  [(or (= (modulo x 2) 0) (= (modulo x 5) 0)) (cons (* -1 x) (lambda () (f (+ x 1))))]
                  [else (cons x (lambda () (f (+ x 1))))]))))
               (lambda () (f 1))))

(define key-heart-star (lambda () (cons key (lambda () (cons heart (lambda () (cons yellow-star key-heart-star)))))))

(define (two-pairs-stream s)
  (letrec ((f (lambda (s)
                (let ([pr (s)])
                  (cons (cons 2 (car pr)) (lambda () (f (cdr pr))))))))
               (lambda () (f s))))

(define (spin-stream xs ys)
  (define (helper x)
     (cons (cons (list-ref xs (modulo x (length xs))) (list-ref ys (modulo x (length ys))))
           (lambda () (helper (add1 x)))))
  (lambda () (helper 0)))

(define (kvpv-lookup v vec)
  (cond [(= (vector-length vec) 0) #f]
        [(not (pair? (vector-ref vec 0))) (kvpv-lookup v (vector-drop vec 1))] ;skip elements that aren't a pair
        [(equal? v (car (vector-ref vec 0))) (vector-ref vec 0)]
        [else (kvpv-lookup v (vector-drop vec 1))]))

(define (helper-cached-lookup k vec lst)
  (let ([cache-check (kvpv-lookup k vec)])
    (cond [(equal? cache-check #f) (assoc k lst)]
          [else (cons #t cache-check)])))
      

(define (cached-lookup lst n)
  (let ([vec (make-vector n #f)] [i 0])
    (lambda (k _) (let ([lookup (helper-cached-lookup k vec lst)])
               (cond
                 [(equal? lookup #f) #f]
                 [(equal? (car lookup) #t) lookup]
                 [else (begin (vector-set! vec (modulo i n) lookup) (add1 i) (cons #f lookup))])))))


; 1)
; notice, the first argument to check-expect is our test, the second argument is the expected result
(check-expect (downseries 2 11 3) '(11 9 7 5 3))

; 2)
(check-expect (meow-string-map '("hi" "hello" "there")) '("himeow" "hellomeow" "theremeow"))

; 3)
; you should probably have one that exceeds the bounds of your list though, like 6
(check-expect (list-ref-div (list 1 2 3) 0) 1)
(check-expect (list-ref-div (list 1 2 3) 3) 2)

; 4)
; this assumes you have nats defined
(check-expect (next-k-items nats 3) '(1 2 3))

; 5)
(check-expect (kth-item nats 3) 3)

; 7)
(check-expect (next-k-items key-heart-star 2) (list key heart))

; 8)
(check-expect (kth-item (two-pairs-stream nats) 2) '(2 . 2))

; 9)
(check-expect (next-k-items (spin-stream '(1 2 3) '("a" "b")) 6)
              '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b")))

; 10)
(check-expect (kvpv-lookup 2 '#((1 . 1) (2 . 1))) '(2 . 1))

; 11)
; this will test if you're getting the expected results from the cached-lookup
(define tstlst '((1 . "a") (2 . "b") (3 . "c") (4 . "d") (5 . "e")))
(define cl-fun (cached-lookup tstlst 3))
(check-expect (cl-fun 6 tstlst) #f)
(check-expect (cl-fun 1 tstlst) '(#f 1 . "a"))
(check-expect (cl-fun 2 tstlst) '(#f 2 . "b"))
(check-expect (cl-fun 3 tstlst) '(#f 3 . "c"))
(check-expect (cl-fun 1 tstlst) '(#t 1 . "a"))
(check-expect (cl-fun 4 tstlst) '(#f 4 . "d"))
(check-expect (cl-fun 1 tstlst) '(#f 1 . "a"))
(check-expect (cl-fun 1 tstlst) '(#t 1 . "a"))

(test)