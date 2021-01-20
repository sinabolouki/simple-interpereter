#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require (except-in eopl #%module-begin))


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
             ("switch" (token-SWCHKW))
             ("case" (token-CASEKW))
             ("break" (token-BRKKW))
             ("default" (token-DEFKW))
             ("print" (token-PRTKW))
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
             ((:or "true" "false") (token-BOOL (if (equal? lexeme "true") #t #f)))
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
(define-tokens e (BOOL))
(define-empty-tokens d (EOF SEMICOL WHLKW DOKW IFKW THENKW ELSKW ENDKW RETKW SWCHKW CASEKW BRKKW DEFKW PRTKW EQ GRT LESS EQCHK NEQCHK ADD SUB MUL DIV OPAR CPAR NULLKW OBRC CBRC COMMA))

(define simple-parser
           (parser
              (start command)
              (end EOF)
              (error void)
              (tokens a b c d e)
              (grammar
                 (command
                     ((keyword) $1)
                     ((command SEMICOL keyword) (list 'command $1 $3))
                  )
                 (keyword
                     ((whl_stmt) $1)
                     ((if_stmt) $1)
                     ((ret_stmt) $1)
                     ((switch_stmt) $1)
                     ((print_stmt) $1)
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
                 (switch_stmt
                     ((SWCHKW aexp case_stmts ENDKW) (list 'switch $2 $3))
                  )
                 (case_stmts
                     ((case_stmt) $1)
                     ((case_stmt case_stmts) (list $1 $2))
                     ((DEFKW command) (list 'default $2))
                  )
                 (case_stmt
                     ((CASEKW constant command) (list 'case $2 $3))
                     ((CASEKW constant command BRKKW) (list 'case $2 $3 'break))
                  )
                 (constant
                     ((NUM) (list 'num $1))
                     ((SUB NUM) (list 'neg $2))
                     ((STR) (list 'str $1))
                  )
                 (print_stmt
                     ((PRTKW exp) (list 'print $2))
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
                     ((BOOL) (list 'bool $1))
                     ((STR) (list 'str $1))
                     ((list) $1)
                     ((VAR listmember) (list 'accessmember $1 $2))
                  )
                 (list
                     ((OBRC CBRC) 'emptylist)
                     ((OBRC listvalues CBRC) (list 'listvalues $2))
                  )
                 (listvalues
                     ((exp) (list 'listval $1))
                     ((exp COMMA listvalues) (list 'listvals $1 $3))
                  )
                 (listmember
                     ((OBRC exp CBRC) (list 'listmember $2))
                     ((OBRC exp CBRC listmember) (list 'listmember $2))
                  )
               )
            )
  )

;test
(define input-string (file->string  "test.txt"))
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-lexer (open-input-string input-string)))
;(define my-lexer (lex-this simple-lexer (open-input-string "if x == 2 then a = 5 else false;     return    [-7, 6]")))
;(define my-lexer (lex-this simple-lexer (open-input-string "return 5;return 6")))
;(define my-lexer (lex-this simple-lexer (open-input-string "a=2;b=3;c=4;return         false")))
;(define my-lexer (lex-this simple-lexer (open-input-string "switch x+2 case 12 a=2 break case 10 q=true case 8 b=d break default print -4 end")))
;(define my-lexer (lex-this simple-lexer (open-input-string "switch x+2 case 12 a=2 break case 10 q=true case 8 b=d case 6 d=b end")))
;(define my-lexer (lex-this simple-lexer (open-input-string "return         false ; a = 5")))
;(define my-lexer (lex-this simple-lexer (open-input-string "if x == 2 then a + 5 else false;     return    [-7, 6]")))
;(define my-lexer (lex-this simple-lexer (open-input-string "return \"abcd\" > 1 + 3 * b[k*(2+5)]")))
;(let ((parser-res (simple-parser my-lexer))) parser-res)

(provide (all-defined-out))