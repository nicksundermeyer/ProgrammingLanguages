#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "LexerClassExample.rkt"
         "AbstractSyntaxTreeClassExample.rkt")

(provide (all-defined-out))

(define myparser
  (parser
      (start prog)
      (end EOF)
      (tokens names-and-values
              end-of-file
              math-operators
              bool-operators
              let-keywords
              lambda-keywords
              keywords
              parens)
      (error (lambda (tok-ok? tok-name tok-value)
               (printf "Parser error: token ~a value ~a"
                        tok-name
                        tok-value)))
      (grammar
             (prog
                 [(LEFTPAREN PLUS prog prog RIGHTPAREN) (plus-expr $3 $4)]
                 [(LEFTPAREN MULTIPLY prog prog RIGHTPAREN) (multiply-expr $3 $4)]
                 [(LEFTPAREN AND prog prog RIGHTPAREN) (and-expr $3 $4)]
                 [(LEFTPAREN OR prog prog RIGHTPAREN) (or-expr $3 $4)]
                 [(LEFTPAREN NOT prog RIGHTPAREN) (not-expr $3)]
                 [(LEFTPAREN LET LEFTPAREN IDENTIFIER NUMERIC RIGHTPAREN prog RIGHTPAREN)
                          (let-expr (identifier-expr $4) (numeric-expr $5) $7)]
                 [(LEFTPAREN LET LEFTPAREN IDENTIFIER BOOLEAN RIGHTPAREN prog RIGHTPAREN)
                          (let-expr (identifier-expr $4) (boolean-expr $5) $7)]
                 [(LEFTPAREN LET LEFTPAREN IDENTIFIER
                             LEFTPAREN LAMBDA LEFTPAREN IDENTIFIER RIGHTPAREN
                             prog RIGHTPAREN RIGHTPAREN
                             prog RIGHTPAREN
                             )
                          (let-expr (identifier-expr $4)
                                    (lambda-expr $8 $10)
                                    $13)]
                 ;; Match a function application
                 [(LEFTPAREN IDENTIFIER prog RIGHTPAREN) (function-app $2 $3)]
                 [(BOOLEAN) (boolean-expr $1)]
                 [(NUMERIC) (numeric-expr $1)]
                 [(IDENTIFIER) (identifier-expr $1)]
                 )
             )
      ))

(define (parse in)
  (myparser (get-tokenizer in)))

(define (parsestr str)
  (let ([in (open-input-string str)])
    (parse in)))

;; Tests used
; (parsestr "(let (f (lambda (x) (plus x 2))) (f 3))")
; (let-expr (identifier-expr "f") (lambda-expr "x" (plus-expr (identifier-expr "x") (numeric-expr 2))) (function-app "f" (numeric-expr 3)))