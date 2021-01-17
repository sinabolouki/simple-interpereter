#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-lexer
           (lexer
             (";" (token-SEMICOL))
             ("while" (token-WHLKW))
             ("do" (token-DOKW))
             ("if" (token-IFKW))
             ("then" (token-THENKW))
             ("else" (token-ELSKW))
             ("end" (token-ENDKW))
             ("return" (token-RETKW))
             ("=" (token-EQ))
             (">" (token-GRT))
             ("<" (token-LESS))
             ("==" (token-EQCHK))
             ("!=" (token-NEQCHK))
             ("+" (token-ADD))
             ("-" (token-SUB))
             ("*" (token-MUL))
             ("/" (token-DIV))
             ("(" (token-OPAR))
             (")" (token-CPAR))
             ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
             ("null" (token-NULL))
             (alphabetic (token-VAR lexeme))
             ("true" (token-TRUE))
             ("false" (token-FALSE))
             ((:: #\" any-string #\") (token-STR lexeme))
             ("[" (token-OBRC))
             ("]" (token-CBRC))
             ("," (token-COMMA))
            
             (whitespace (simple-lexer input-port))
             ((eof) (token-EOF))
            )
  )

(define-tokens a (NUM))
(define-tokens b (VAR))
(define-tokens c (STR))
(define-empty-tokens d (EOF SEMICOL WHLKW DOKW IFKW THENKW ELSKW ENDKW RETKW EQ GRT LESS EQCHK NEQCHK ADD SUB MUL DIV OPAR CPAR NULL TRUE FALSE OBRC CBRC COMMA))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-lexer (open-input-string "if x == 2 then a + 5 else false;     return    [-7, 6]")))
(define my-lexer2 (lex-this simple-lexer (open-input-string "return \"abcd\" > 1 + 3 * b[k*(2+5)]")))