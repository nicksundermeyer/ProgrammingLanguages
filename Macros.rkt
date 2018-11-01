#lang racket

(struct mult (e1 e2) )

(define-syntax my-if
  (syntax-rules (then else) ; additional or new keywords
    [(my-if e1 then e2 else e3) (if e1 e2 e3)]
    ))

(my-if (= 5 2) then "hello" else "goodbye")