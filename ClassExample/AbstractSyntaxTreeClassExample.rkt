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
(struct function-app (name param) #:transparent)

(struct lambda-expr (parameter body) #:transparent)
(struct closure (parameter body env) #:transparent) ; basically a lambda-expr plus an environment, to remember the environment at the time it was defined

(struct let-expr (id value body) #:transparent)