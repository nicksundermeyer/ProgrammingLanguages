#lang racket

(require "dbn-ast.rkt" "dbn-parser.rkt" "papersim.rkt" "dbn-env.rkt" "dbn-errors.rkt")



; generic evaluate from an input port
(define (eval-dbn in [env #f] [run-sim #t] [clear? #t])
  ; reset errors before we move on
  (reset-errors!)
  ; this creates an ast from in
  (let ([prog (parse in)])
    (if (not (parser-error))
        ; see if we should run the sim
        (begin
          (when (eq? run-sim #t)
            (run-paper-sim))
          (set-antialias 'aligned)
          ; evaluate the statements
          (eval-statements (empty-env) (program-statements prog))
          ; refresh to make sure things draw
          (refresh))
        (error "quitting due to parse error"))))

; evaluate a file
(define (eval-file filename [env #f] [run-sim #t] [clear? #t])
  (let ([in (open-input-file filename)])
    (run-paper-sim)
    (eval-dbn in env run-sim clear?)
    (refresh)))

; evaluate a string
(define (eval-str str)
  (let ([in (open-input-string str)])
    (eval-dbn in)))

; evaluates all the files under a directory
(define (eval-dir dirname sleeplen [env #f] [run-sim #t] [clear? #t])
  (run-paper-sim)
  (fold-files (λ (path kind acc)
                (cond
                  [(eq? kind 'file)
                   (printf "Evaluating ~a~n" path)
                   (eval-file path env #t clear?)
                   (refresh)
                   (printf "Enter for next one:")
                   (read (current-input-port))])) #f dirname #f))

; evaluate the list of statements
(define (eval-statements env statements [slow? #f])
  ; this is simply a fold, but we pass the environment along
  ; in the accumulator so we can update it from statement to statement
  ; as needed, this means sometimes we have to keep it! notice that
  ; foldl takes the arguments opposite of foldl in Haskell (accumulator
  ; comes last, not first
  (unless (not slow?) (sleep slow?))
  (foldl-and-exit (λ (s e) (eval-statement e s #f)) env statements))
  

; this is like foldl, but you can exit
(define (foldl-and-exit fun acc lst)
  ; if the list is empty, return the accumulator
  (if (null? lst)
      acc
      ; otherwise, recurse
      (let* ([el (first lst)]
             [result (fun el acc)])
        (match result
          [(cons 'exit v) v]
          [else (foldl-and-exit fun result (rest lst))]))))

(define (add-to-env env names vals)
  (cond [(null? names) env]
        [else (add-to-env (extend-env env (first names) (first vals)) (rest names) (rest vals))]))

; This function evaluates statements, but it also accumulates the
; environment, meaning that it will pass the environment from fun
; to fun
(define (eval-statement env statement [slow? #f])
  (unless (not slow?) (sleep slow?))
  (match statement
    ; Paper 
    [(paper-expr exp xs ys) 
     (clear-paper (dbncolor (eval-expr env exp))) env]    
    ; Pen
    [(pen-expr exp)
     (set-pen-color! (dbncolor (eval-expr env exp))) env]
    ; Print
    [(print-expr exp) (printf "~a~n" (eval-expr env exp)) env]

    ; line-expr: takes four points as arguments, evaluate each for numeric value, then draws line between points
    [(line-expr x1 x2 x3 x4) (draw-line (eval-expr env x1) (eval-expr env x2) (eval-expr env x3) (eval-expr env x4)) env]
    
    ; Assignment to a paper location, this is a special case
    [(assignment-expr (get-paper-loc x y) color)
     (let ([xcoord (eval-expr env x)]
           [ycoord (eval-expr env y)]
           [col (eval-expr env color)])
       (draw-point xcoord ycoord (dbncolor col)))
     env]
       

    ; Assignment to a variable name, need to see if it's there first
    ; assignment-expr: check environment for variable ID, if it's not there then create it, otherwise just setref
    [(assignment-expr (var-expr e1) e2)
    (let ([var (apply-env env e1)]
          [val (eval-expr env e2)])
      (if var
          (setref! var val)
          (extend-env env e1 val)))]
     
    
    ; the antialias expression, for setting up antialias stuff
    [(antialias-expr expr)
     (let ([val (eval-expr env expr)])
       (cond [(= val 0) (set-antialias 'aligned)]
             [(< val 0) (set-antialias 'unaligned)]
             [else (set-antialias 'smoothed)]))]

    ; Repeat!
    [(repeat-expr sym from to body)
     (let* ([start (eval-expr env from)] ; evaluate the start value
            [end (eval-expr env to)]     ; then the ending value
            [newenv (extend-env env sym start)]
            [ref (apply-env newenv sym)])
       (letrec ([loop (λ () (cond [(<= (deref ref) end)                                   
                                   (eval-statements newenv body slow?)
                                   (setref! ref (add1 (deref ref)))
                                   ;(printf "repeat from ~a to ~a~n" (deref ref) end)
                                   (refresh)
                                   (loop)]))])
         (loop)
         env))]

    ; Forever loops
    [(forever-expr body)
     ; just loop forever, reuse the env from when we entered the loop
     (letrec ([loop (λ ()
                      (eval-statements env body slow?)
                      (refresh)
                      (loop))])
       (loop)
       env)]

    ; boolean-like expressions--we execute the body depending on whether or not they return true
    ; Same? 
    [(same-expr expr1 expr2 body)
     (let ([val1 (eval-expr env expr1)]
           [val2 (eval-expr env expr2)])       
       (when (equal? val1 val2) (eval-statements env body slow?))
       env)]
    
    ; NotSame?
    [(not-same-expr expr1 expr2 body)
     (let ([val1 (eval-expr env expr1)]
           [val2 (eval-expr env expr2)])
       (when (not (equal? val1 val2)) (eval-statements env body slow?))
       env)]

    ; Smaller?
    [(smaller-expr expr1 expr2 body)
     (let ([val1 (eval-expr env expr1)]
           [val2 (eval-expr env expr2)])
       (when (< val1 val2) (eval-statements env body slow?))
       env)]

    ; NotSmaller?
    [(not-smaller-expr expr1 expr2 body)
     (let ([val1 (eval-expr env expr1)]
           [val2 (eval-expr env expr2)])
       (when (not (< val1 val2)) (eval-statements env body))
       env)]

    ; Value statement, this is like a return, so we don't need to
    ; pass on the current environment since it should really be the
    ; last thing done in a list of statements
    [(value-expr expr) (cons 'exit (eval-expr env expr))]
    
    
    ; to create a function, we need to create a closure and store it in the
    ; current environment, so a new environment will be passed on here
    ; command-fun: creates a function by making a closure, storying it in the environment, and returning the new environment
    [(command-fun sym params body)
     (let ([close (closure sym params body env)])
       (let ([newenv (extend-env env sym close)])
         newenv))]
     
    ; and we do the same thing for the numbers
    ;;; TODO (Achievement) [(number-fun sym params body)
     

    ; now for expressions as statements, these we ignore the return value of
    ;;; application as statements, I've left some comments to help you along
    ; [(apply-expr sym exprs)  
     ; evaluate all the arguments, then call the function
         ; make sure we found it, or return an error otherwise
       ; return the previous environment to be carried along

    ; apply-expr: applies a function by looking up the function name in the environment, evaluating all the arguments
    ; of the function into a list, extending the environment by walking through both these lists and extending the
    ; environment with each element, and finally evaluating the function body with this new environment
    
    [(apply-expr sym exprs)
     (let ([ref (apply-env env sym)])
       (if ref
           (let ([param-names (closure-params (deref ref))]
                 [vals (map (λ (arg) (eval-expr env arg)) exprs)])
               (eval-statements (add-to-env env param-names vals) (closure-body (deref ref))))
           (error "undefined ref"))) env]
    ))

(define (eval-expr env expr)
  (match expr
    ; literal numbers
    [(numeric-expr a) a]

    ; variable lookups
    [(var-expr sym) (let [(val (apply-env env sym))]
                      (if val
                          (deref val)
                          (error "undefined variable " sym)))]
    
    ; used as an expression, the paper location returns the color
    [(get-paper-loc x y)
     (let* ([xcoord (eval-expr env x)]
            [ycoord (eval-expr env y)]
            [color (get-pixel-color xcoord ycoord)])
       color)]

    ; math operations
    [(add-expr a b) (+ (eval-expr env a) (eval-expr env b))]
    [(sub-expr a b) (- (eval-expr env a) (eval-expr env b))]
    [(div-expr a b) (/ (eval-expr env a) (eval-expr env b))]
    [(mult-expr a b) (* (eval-expr env a) (eval-expr env b))]

    ; read mouse info
    [(mouse-expr expr)
     (let ([val (eval-expr env expr)])
       (cond
         [(= val 1) (get-mouse-x)]
         [(= val 2) (get-mouse-y)]
         [(= val 3) (get-mouse-button)]
         [else (error "Expected a mouse value from 1 to 3 inclusive, got " val " instead.")]))]

    ; read key info
    [(key-expr expr)
     (let ([val (eval-expr env expr)])
       ; uh, this seems a little limiting, we can only read 26 keys?
       (cond [(and (>= val) (<= val 26)) (get-key val)]
             [else (error "Expected a key range from 1 to 26 inclusive, got " val " instead.")]))]
    

    ; time expressions
    [(time-expr expr)
     (let ([val (eval-expr env expr)])
       (cond [(= val 1) (get-time 'hour)]
             [(= val 2) (get-time 'minutes)]
             [(= val 3) (get-time 'seconds)]
             [(= val 4) (get-time 'milliseconds)]
             [else (error "Expected a Time range from 1 to 4 inclusive, got " val " instead")]))]
                        

    ; handle function application as an expression, these we care about the return value
    ;;; function application as an expression (not a statement)--you should return
    ; the result of the evaluation of all the statements in the body
    ; [(apply-expr sym exprs)
     ; evaluate all the arugments, then call the function
         ; make sure we found it, or return an error otherwise
                  ; grab the closure from the environment, which has parameters
                     ; then evaluate all the statements and return the result
    [(apply-expr sym exprs)
     (let ([ref (apply-env env sym)])
       (if ref
           (let ([param-names (closure-params (deref ref))]
                 [vals (map (λ (arg) (eval-expr env arg)) exprs)])
               (let ([result (eval-statements (add-to-env env param-names vals) (closure-body (deref ref)))]) result))
           (error "undefined ref")))]
    ))