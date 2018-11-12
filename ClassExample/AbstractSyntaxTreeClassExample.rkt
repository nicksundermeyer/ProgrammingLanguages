#lang racket

(provide (all-defined-out))

(struct plus-expr (argone argtwo) #:transparent)
(struct multiply-expr (argone argtwo) #:transparent)
(struct and-expr (argone argtwo) #:transparent)
(struct or-expr (argone argtwo) #:transparent)
(struct not-expr (argone) #:transparent)
(struct numeric-expr (argone) #:transparent)
(struct boolean-expr (argone) #:transparent)
(struct identifier-expr (argone) #:transparent)

(struct lambda-expr (parameter body) #:transparent)

(struct let-expr (id value body) #:transparent)