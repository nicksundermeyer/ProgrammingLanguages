#lang racket

(require 2htdp/planetcute)
(require test-engine/racket-tests)

; natural numbers, for testing
(define nats (letrec ((f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))))
               (lambda () (f 1))))

; 1. downseries - takes three arguments, step, high, and low (all numbers), and produces a list of numbers from high to low separated by step, sorted in descending order
(define (downseries step high low)
  (cond
    [(< high low) empty]
    [else (cons high (downseries step (- high step) low))]))

; 2. meow-string-map - takes a list of strings, and appends "meow" to every string in the list
(define (meow-string-map lst)
  (map (lambda (string) (string-append string "meow")) lst))

; 3. list-ref-div - takes a list and a number n, returns the ith element where i is the quotient of n/list length
(define (list-ref-div lst n)
  (cond
    [(null? lst) (error "list-ref-div: the list is empty")]
    [(< n 0) (error "list-ref-div: negative number")]
    [else (list-ref lst (quotient n (length lst)))]))

; 4. next-k-items - takes a stream and a number k, and returns a list containing the next k elements of the stream
(define (next-k-items s k)
  (letrec ([f (lambda (s k ans)
                (let ([pr (s)])
                  (if (<= k 0)
                      (reverse ans)
                      (f (cdr pr) (- k 1) (cons (car pr) ans)))))])
    (f s k '() )))

; 5. kth-items - takes a stream and a number k, and returns the result of extracting k elements from the stream and taking the last one
(define (kth-item s k)
  (list-ref (next-k-items s k) (- k 1)))

; 6. negate-2-and-5 - generates a stream like the natural numbers, but with multiples of 2 and 5 negated
(define negate-2-and-5
  (letrec ((f (lambda (x)
                (cond
                  [(or (= (modulo x 2) 0) (= (modulo x 5) 0)) (cons (* -1 x) (lambda () (f (+ x 1))))]
                  [else (cons x (lambda () (f (+ x 1))))]))))
               (lambda () (f 1))))
; 7. key-heart-start - generates a stream where elements alternate between key, heart, and yellow-star
(define key-heart-star (lambda () (cons key (lambda () (cons heart (lambda () (cons yellow-star key-heart-star)))))))

; 8. two-pairs-stream - takes a stream and returns a new stream where each element is 2 paired with the kth element of the stream
(define (two-pairs-stream s)
  (letrec ((f (lambda (s)
                (let ([pr (s)])
                  (cons (cons 2 (car pr)) (lambda () (f (cdr pr))))))))
               (lambda () (f s))))

; 9. spin-stream - takes two lists xs and ys, and returns a stream which returns pairs of elements from each list, rotating through the lists forever
(define (spin-stream xs ys)
  (define (helper x)
     (cons (cons (list-ref xs (modulo x (length xs))) (list-ref ys (modulo x (length ys))))
           (lambda () (helper (add1 x)))))
  (lambda () (helper 0)))

; 10. kvpv-lookup - takes a value v and vector vec, locates the first element of the vector whose car is equal to v
(define (kvpv-lookup v vec)
  (cond [(= (vector-length vec) 0) #f]
        [(not (pair? (vector-ref vec 0))) (kvpv-lookup v (vector-drop vec 1))] ;skip elements that aren't a pair
        [(equal? v (car (vector-ref vec 0))) (vector-ref vec 0)]
        [else (kvpv-lookup v (vector-drop vec 1))]))

; 11. cached-lookup - takes a list and a number n, and looks up the number in the list using a cached method by first looking them up in the cache, then the list
(define (cached-lookup lst n)
  (let ([vec (make-vector n)] [i 0]) ; make cache vector and i value to iterate through
    (lambda (k _)
      (let ([lookup (cached-lookup-helper k vec lst)]) ; lookup from cache and list
               (cond
                 [(equal? lookup #f) #f]
                 [(equal? (car lookup) #t) lookup] 
                 [else (begin ; add to cache if found in list
                         (vector-set! vec (modulo i n) lookup)
                         (add1 i)
                         (cons #f lookup))])))))

; helper method to check cache for cached-lookup
(define (cached-lookup-helper k vec lst)
  (cond
    [(equal? (kvpv-lookup k vec) #f) (assoc k lst)] ; value not found in cache
    [else (cons #t (kvpv-lookup k vec))])) ; value found in cache