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
             ("null" (token-NULLKW))
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
(define-empty-tokens d (EOF SEMICOL WHLKW DOKW IFKW THENKW ELSKW ENDKW RETKW EQ GRT LESS EQCHK NEQCHK ADD SUB MUL DIV OPAR CPAR NULLKW TRUE FALSE OBRC CBRC COMMA))

(define simple-parser
           (parser
              (start command)
              (end EOF)
              (error void)
              (tokens a b c d)
              (grammar
                 (command
                     ((keyword) $1)
                     ((command SEMICOL keyword) (list $1 $3))
                  )
                 (keyword
                     ((whl_stmt) $1)
                     ((if_stmt) $1)
                     ((ret_stmt) $1)
                     ((asgn_stmt) $1)
                  )
                 (whl_stmt
                     ((WHLKW exp DOKW command ENDKW) (list 'while $2 $4))
                  )
                 (if_stmt
                     ((IFKW exp THENKW command ELSKW command ENDKW) (list 'if $2 $4 $6))
                  )
                 (ret_stmt
                     ((RETKW exp) (list 'ret $2))
                  )
                 (asgn_stmt
                     ((VAR EQ exp) (list 'eq $1 $3))
                  )
                 (exp
                     ((aexp) $1)
                     ((aexp GRT aexp) (list 'grt $1 $3))
                     ((aexp LESS aexp) (list 'less $1 $3))
                     ((aexp EQCHK aexp) (list 'iseq $1 $3))
                     ((aexp NEQCHK aexp) (list 'isneq $1 $3))
                  )
                 (aexp
                     ((bexp) $1)
                     ((bexp SUB aexp) (list 'sub $1 $3))
                     ((bexp ADD aexp) (list 'add $1 $3))
                  )
                 (bexp
                     ((cexp) $1)
                     ((cexp MUL bexp) (list 'mul $1 $3))
                     ((cexp DIV bexp) (list 'div $1 $3))
                  )
                 (cexp
                     ((SUB cexp) (list 'neg $2))
                     ((OPAR exp CPAR) $2)
                     ((NUM) (list 'num $1))
                     ((NULLKW) (list 'null))
                     ((VAR) (list 'var $1))
                     ((TRUE) (list 'bool 'true))
                     ((FALSE) (list 'bool 'false))
                     ((STR) (list 'str $1))
                     ((list) $1)
                     ((VAR listmember) (list 'accessmember $1 $2))
                  )
                 (list
                     ((OBRC CBRC) 'emptylist)
                     ((OBRC listvalues CBRC) (list 'list $2))
                  )
                 (listvalues
                     ((exp) $1)
                     ((exp COMMA listvalues) (list $1 $3))
                  )
                 (listmember
                     ((OBRC exp CBRC) $2)
                     ;other
                  )
               )
            )
  )

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-lexer (open-input-string "return         false ; a = 5")))
;(define my-lexer (lex-this simple-lexer (open-input-string "if x == 2 then a + 5 else false;     return    [-7, 6]")))
;(define my-lexer (lex-this simple-lexer (open-input-string "return \"abcd\" > 1 + 3 * b[k*(2+5)]")))
(let ((parser-res (simple-parser my-lexer))) parser-res)