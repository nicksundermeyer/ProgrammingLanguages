#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-empty-tokens parens (LEFTPAREN RIGHTPAREN))
(define-empty-tokens bool-operators (AND OR NOT))
(define-empty-tokens math-operators (PLUS MULTIPLY))
(define-empty-tokens comparison-operators (SAME
                                           NOTSAME
                                           SMALLER
                                           NOTSMALLER))
(define-empty-tokens if-keywords (IF THEN ELSE))
(define-empty-tokens let-keywords (LET))
(define-empty-tokens lambda-keywords (LAMBDA))
(define-empty-tokens end-of-file (EOF))

(define-empty-tokens keywords (bool-operators
                               math-operators
                               if-keywords
                               lambda-keywords
                               comparison-operators))

(define-tokens names-and-values
  (IDENTIFIER NUMERIC BOOLEAN))

(define mylexer
  (lexer
    [#\(               (token-LEFTPAREN)]
    [#\)               (token-RIGHTPAREN)]
    [(:or "And" "and") (token-AND)]
    [(:or "Or" "or")   (token-OR)]
    [(:or "Not" "not") (token-NOT)]
    [(:or "Plus" "plus") (token-PLUS)]
    [(:or "Multiply" "multiply") (token-MULTIPLY)]
    [(:or "Let" "let") (token-LET)]
    [(:or "Same" "same") (token-SAME)]
    [(:or "NotSame" "notsame") (token-NOTSAME)]
    [(:or "If" "if")   (token-IF)]
    [(:or "Then" "then") (token-THEN)]
    [(:or "Else" "else") (token-ELSE)]
    [(:or "True" "true") (token-BOOLEAN true)]
    [(:or "False" "false") (token-BOOLEAN false)]
    [(:or "Lambda" "lambda") (token-LAMBDA)]
    [(:+ numeric) (token-NUMERIC (string->number lexeme))]
    [(:: (:+ alphabetic) (:* (:or numeric alphabetic)))
          (token-IDENTIFIER lexeme)]
    [whitespace (mylexer input-port)]
    [(eof)             (token-EOF)]
    ))

(define (get-tokenizer in)
  (λ () (mylexer in)))

(define (lex in)
  (let ([tokenizer (get-tokenizer in)])
    (define (lex-function)
      (let ([tok (tokenizer)])
        (cond
          [(eq? tok (token-EOF)) null]
          [else (cons tok (lex-function))])))
    (lex-function)))

(define (lexstr str)
  (lex (open-input-string str)))
