#lang racket

; build a language that does boolean and numeric expressions
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-empty-tokens parens (LEFTPAREN RIGHTPAREN))
(define-empty-tokens bool-operators (AND OR NOT))
(define-empty-tokens math-operators (PLUS MULTIPLY))
(define-empty-tokens comparison-operators (SAME NOTSAME SMALLER NOTSMALLER))
(define-empty-tokens if-keywords (IF THEN ELSE))
(define-empty-tokens end-of-file (EOF))

(define-tokens names-and-values
  (NUMERIC BOOLEAN))

(define mylexer
  (lexer
   (#\( (token-LEFTPAREN))
   (#\) (token-RIGHTPAREN))
   ((:or "And" "and") (token-AND))
   ((:or "Or" "or") (token-OR))
   ((:or "Not" "not") (token-NOT))
   ((:or "If" "if") (token-IF))
   ((:or "Then" "then") (token-THEN))
   ((:or "Else" "else") (token-ELSE))
   ((:or "Plus" "plus") (token-PLUS))
   ((:or "Multiply" "multiply") (token-MULTIPLY))
   ((:or "Same" "same") (token-SAME))
   ((:or "NotSame" "notsame") (token-NOTSAME))
   ((:or "True" "true") (token-BOOLEAN true))
   ((:or "False" "false") (token-BOOLEAN false))
   ((:+ numeric) (token-NUMERIC (string->number lexeme)))
   (whitespace (mylexer input-port))
   ((eof) (token-EOF))
   ))

; lexer returns a stream, which is hard to work with due to difficulty of accessing specific tokens
; helper functions to make it easier to deal with

; produces function to get tokens out of input stream
(define (get-tokenizer in)
  (λ () (mylexer in)))

; gets elements out of stream and turns them into a list
(define (lex in)
  (let ((tokenizer (get-tokenizer in)))
    (define (lex-function)
      (let ((tok (tokenizer))) ; invoke stream and get the next thing out of it, assign to local variable
        (cond
          [(eq? tok (token-EOF)) null]
          [else (cons tok (lex-function))])))
    (lex-function)))

(define (lexstr str)
  (lex (open-input-string str))) ; produces an input port allowing us to read characters one at a time