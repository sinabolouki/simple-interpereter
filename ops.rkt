#lang racket

(require (except-in eopl #%module-begin ))

(define (is-lis-num lis)
  (cond
    [(null? lis) #t]
    [(number? (car lis)) (is-lis-num (cdr lis))]
    [else #f]
    ))

(define (is-lis-boolean lis)
 (cond
   [(null? lis) #t]
   [(boolean? (car lis)) (is-lis-boolean (cdr lis))]
   [else #f]
   ))

(define (is-lis-str lis)
 (cond
   [(null? lis) #t]
   [(string? (car lis)) (is-lis-str (cdr lis))]
   [else #f]
   ))

(define (gr-ls-num lis number)
  (cond
    [(null? lis) #t]
    [(boolean=? (is-lis-num lis) #f) (eopl:error "list elements are not number")]
    [(<= (car lis) number) #f]
    [else (gr-ls-num (cdr lis) number)]
    ))

(define (le-ls-num lis number)
  (cond
    [(null? lis) #t]
    [(boolean=? (is-lis-num list) #f ) (eopl:error "list elements are not number")]
    [(<= (car lis) number) #f]
    [else (le-ls-num (cdr lis) number)]
    ))

(define (gr-ls-str lis string)
  (cond
    [(null? lis) #t]
    [(boolean=? (is-lis-str list) #f) (eopl:error "list elements are not string")]
    [(string<=? (car lis) string) #f]
    [else (gr-ls-str (cdr lis) string)]
    ))

(define (le-ls-str lis string)
  (cond
    [(null? lis) #t]
    [(boolean=? (is-lis-str list) #f) (eopl:error "list elements are not string")]
    [(string>=? (car lis) string) #f]
    [else (le-ls-str (cdr lis) string)]
    ))

(define (ls-eq? lis1 lis2)
  (cond
    [(and (not (null? lis1)) (null? lis2)) #f]
    [(and (null? lis1) (not(null? lis2))) #f]
    [(and (null? lis1)(null? lis2)) #t]
    [(and (number? (car lis1)) (equal? (car lis1) (car lis2))) (ls-eq? (cdr lis1)(cdr lis2))]
    [(and (string? (car lis1)) (string=? (car lis1) (car lis2))) (ls-eq? (cdr lis1)(cdr lis2))]
    [(and (boolean? (car lis1)) (string=? (car lis1) (car lis2))) (ls-eq? (cdr lis1)(cdr lis2))]
    [(and (list? (car lis1)) (ls-eq? (car lis1) (car lis2))) (ls-eq? (cdr lis1)(cdr lis2))]
    [else #f]
    ))

(define (bool-or bool1 bool2) (or bool1 bool2))
(define (bool-and bool1 bool2)(and bool1 bool2))

(define (lis-operand-num lis op number)
  (cond
    [(null? lis) null]
    [(boolean=? (is-lis-num lis) #f) (eopl:error "list elements are not number")]
    [else (append (list(arith-op (car lis) op number)) (lis-operand-num (cdr lis) op number))]
    ))

(define (lis-operand-bool lis op number)
  (cond
    [(null? lis) null]
    [(boolean=? (is-lis-boolean lis) #f) (eopl:error "list elements are not boolean")]
    [else (append (list (arith-op (car lis) op number)) (lis-operand-bool (cdr lis) op number))]
    ))

(define (string-plus-lis string lis)
  (cond
    [(null? lis) null]
    [(boolean=? (is-lis-str lis) #f) (eopl:error "list elements are not string")]
    [(null? lis) null]
    [else (append (list (string-append (car lis) string )) (string-plus-lis (cdr lis) string))]
    ))


(define (equality? arg1 arg2)
  (cond
    [(and (null? arg1) (null? arg2)) #t]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #f]    
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #t) (boolean=? arg2 #t)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #t) (boolean=? arg2 #f)) #f]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #f) (boolean=? arg2 #f)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #f) (boolean=? arg2 #t)) #t]
    [(and (list? arg1) (list? arg2)) (ls-eq? arg1 arg2)]
    [else (eopl:error "two types are not comparable.")]
    ))

(define (inequality? arg1 arg2) (not (equality? arg1 arg2)))

(define (negate arg)
  (cond
    [(number? arg) (* arg -1)]
    [(boolean? arg) (not arg)]
    [(list? arg) (lis-operand-num arg "*" -1)]
    [else (eopl:error "argument not negatable")]
    ))

(define (gr? arg1 arg2 )
  (cond
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #t]
    [(and (list? arg1) (number? arg2)) (gr-ls-num arg1 arg2)]
    [(and (number? arg1) (list? arg2))(gr-ls-num arg2 arg1)]
    [(and (list? arg1) (string? arg2)) (gr-ls-str arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (gr-ls-str arg2 arg1)]
    [else (eopl:error "not comparable arguments")]
    ))

(define (ls? arg1 arg2)
  (cond
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #t]
    [(and (list? arg1) (number? arg2)) (le-ls-num arg1 arg2)]
    [(and (number? arg1) (list? arg2)) (le-ls-num arg2 arg1)]
    [(and (list? arg1) (string? arg2)) (le-ls-str arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (le-ls-str arg2 arg1)]
    [else (eopl:error "not comparable arguments"0)]
    ))

(define (arith-op arg1 arg2 arg3)
  (cond
    [(and (number? arg1) (number? arg3) (string=? arg2 "*")) (* arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "+")) (+ arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "-")) (- arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "/") (not(= arg3 0))) (/ arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "/") (= arg3 0)) (eopl:error "Division By Zero")]
    [(and (number? arg1) (list? arg3)) (lis-operand-num arg3 arg2 arg1)]
    [(and (list? arg1) (number? arg3)) (lis-operand-num arg1 arg2 arg3)]
    [(and (boolean? arg1) (boolean? arg3) (string=? arg2 "*")) (bool-and arg1 arg3)]
    [(and (boolean? arg1) (boolean? arg3) (string=? arg2 "+")) (bool-or arg1 arg3)]
    [(and (boolean? arg1) (list? arg3)) (lis-operand-bool arg3 arg2 arg1)]
    [(and (list? arg1) (boolean? arg3)) (lis-operand-bool arg1 arg2 arg3)]
    [(and (string? arg1) (string? arg3) (string=? arg2 "+")) (string-append arg1 arg3)]
    [(and (list? arg1) (list? arg3) (string=? arg2 "+")) (append arg1 arg3)]
    [(and (string? arg1) (list? arg3) (string=? arg2 "+")) (string-plus-lis arg1 arg3)]
    [(and (list? arg1) (string? arg3) (string=? arg2 "+")) (string-plus-lis arg3 arg1)]
    [else (eopl:error "Operand can't be applied on these arguements.")]
    ))
(provide (all-defined-out))

    