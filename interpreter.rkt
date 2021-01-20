#lang racket

(require racket/include)
(require "environment.rkt")
(require "myParser.rkt")
(require "ops.rkt")
(require (except-in eopl #%module-begin))


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define value-of
  (lambda (tree env)
    (let ((action (car tree)))
      (cond
        [(equal? action 'command)
         (let ((result (value-of (cadr tree) env)))
           (if (equal? (caddr result) 'END)
               (list (car result) (cadr result) 'END)
               (value-of (caddr tree) (cadr result)))
          )]
        
        [(equal? action 'if)
         (let* ((exp-result (value-of (cadr tree) env))
                (new-env (cadr exp-result)))
           (if (car exp-result)
               (value-of (caddr tree) new-env)
               (value-of (cadddr tree) new-env))
           )
         ]

        [(equal? action 'while)
         (let ((exp-result (value-of (cadr tree) env)))
           (if (and (car exp-result) (not (eqv? (caddr exp-result) 'END)))
               (let* ((new-env (cadr exp-result))
                      (command-result (value-of (caddr tree) new-env)))
                 (if (not (eqv? (caddr command-result) 'END))
                     (let ((condition-new-env (cadr command-result)))
                       (value-of tree condition-new-env))
                      command-result)
                 )
                 exp-result
             )
           )
         ]

        [(equal? action 'ret)
         (let ((result (value-of (cadr tree) env)))
           (list (car result) (cadr result) 'END)
           )
         ]

        [(equal? action 'eq)
         (let ((result (value-of (caddr tree) env)))
           (list (car result) (extend-env (cadr tree) (car result) (cadr result)) 'NOTEND)
           )
         ]

        [(equal? action 'print)
         (let ((result (value-of (cadr tree) env)))
           (display (car result))
           (display "\n")
           (list (car result) (cadr result) 'NOTEND)
           )
         ]

        [(equal? action 'switch)
         (let* ((exp-val (car (value-of (cadr tree) env)))
                (case-result (value-of (caddr tree) env))
                (cases-list (caddr tree))
                )
           (let ((x (do ((case-result (value-of (caddr tree) env) (if (null? (cdddr cases-list)) (value-of cases-list env) (value-of (cadddr cases-list) env))) (cases-list (caddr tree) (if (null? (cdddr cases-list)) cases-list (cadddr cases-list))))
             ((or (equal? (car case-result) exp-val) (equal? (car cases-list) 'single-case)) (list cases-list case-result))
             )))
           (let* ((cases-list (car x))
                  (case-result (cadr x)))
           (if (equal? (car cases-list) 'single-case)
               (if (equal? (car case-result) exp-val)
                   (value-of (caddr cases-list) env)
                   (value-of (cadddr tree) env))
               (value-of (caddr cases-list) env)
           ))))
         ]

        [(equal? action 'single-case)
         (let ((result (value-of (cadr tree) env)))
           (list (car result) (cadr result) 'NOTEND)
           )
         ]

        [(equal? action 'multi-case)
         (let ((result (value-of (cadr tree) env)))
           (list (car result) (cadr result) 'NOTEND)
           )
         ]

        [(equal? action 'aexp) (value-of (cadr tree) env)]

        [(equal? action 'grt?)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (gr? left-operand right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'less?)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (ls? left-operand right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'iseq?)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (equality? left-operand right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'isneq?)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (inequality? left-operand right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'bexp) (value-of (cadr tree) env)]

        [(equal? action 'sub)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (arith-op left-operand "-" right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'add)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (arith-op left-operand "+" right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'cexp) (value-of (cadr tree) env)]

        [(equal? action 'mul)
         (cond
           ((equal? (car (value-of (cadr tree) env)) 0) (list 0 env 'NOTEND))
           ((equal? (car (value-of (cadr tree) env)) #f) (list #f 'NOTEND))
           (else (list (arith-op (car (value-of (cadr tree) env)) "*" (car (value-of (caddr tree) env))) (cadr (value-of (caddr tree) env))) 'NOTEND))
        ]

        [(equal? action 'div)
         (let* ((valu (value-of (cadr tree) env))
           (left-operand (car valu))
           (new-env (cadr valu))
           (val-left (value-of (caddr tree) env))
           (right-operand (car val-left))
           (second-new-env (cadr val-left)))
           (list (arith-op left-operand "/" right-operand) second-new-env 'NOTEND)
           )
        ]

        [(equal? action 'neg)
         (let* ((valu (value-of (cadr tree) env))
           (oper (car valu))
           (new-env (cadr valu)))
           (list (negate oper) new-env 'NOTEND)
           )
        ]

        [(equal? action 'par-exp) (value-of (cadr tree) env)]
        [(equal? action 'num) (list (cadr tree) env 'NOTEND)]
        [(equal? action 'null) (list '() env 'NOTEND)]
        [(equal? action 'bool) (list (cadr tree) env 'NOTEND)]
        [(equal? action 'str) (list (cadr tree) env 'NOTEND)]
        [(equal? action 'var) (list (apply-env env (cadr tree)) env 'NOTEND)]
        [(equal? action 'list) (value-of (cadr tree) env 'NOTEND)]

        [(equal? action 'accessmember)
         (let* ((result-indices (value-of (caddr tree) env))
                (indices (car result-indices))
                (new-env (cadr result-indices))
                (input-list (apply-env env (cadr tree))))
           (list (get-list-item input-list indices) new-env 'NOTEND)
           )
         ]

        [(equal? action 'list-values) (value-of (cadr tree) env)]
        [(equal? action 'empty-list) (list '() env 'NOTEND)]

        [(equal? action 'list-val)
         (let ((result (value-of (cadr tree) env)))
           (list (list (car result)) (cadr result) 'NOTEND)
           )
         ]
        [(equal? action 'list-vals)
         (let* ((result-val (value-of (cadr tree) env))
                (result-vals (value-of (caddr tree) (cadr result-val))))
           (list (cons (car result-val) (car result-vals)) (cadr result-vals) 'NOTEND)
           )
         ]

        [(equal? action 'list-member)
         (let ((result (value-of (cadr tree) env)))
           (list (list (car result)) (cadr result) 'NOTEND)
           )
         ]
        [(equal? action 'list-members)
         (let* ((result-val (value-of (cadr tree) env))
                (result-vals (value-of (caddr tree) (cadr result-val))))
           (list (cons (car result-val) (car result-vals)) (cadr result-vals) 'NOTEND)
           )
         ]
        ))))

(define one-d-list-ref
  (lambda (lst place)
    (cond
      [(not (list? lst)) (report-not-a-list lst)]
      [(<= (length lst) place) (report-index-out-of-bound lst place)]
      [else (if (= place 0)
            (car lst)
            (one-d-list-ref (cdr lst) (- place 1)))]
      )
    )
  )

(define report-index-out-of-bound
  (lambda (lst index)
    (eopl:error 'apply-env "List ~s Out of Bounds for Index ~s" lst index)
    )
  )

(define report-not-a-list
  (lambda (lst)
    (eopl:error 'apply-env "~s Is Not a List" lst)
    )
  )

(define get-list-item
  (lambda (input-list indices)
    (if (zero? (length indices))
        input-list
        (get-list-item (one-d-list-ref input-list (car indices)) (cdr indices))
      )
    )
  )

(define (while condition body)
  (when condition
    body
    (while condition body)
    )
  )

(define evaluate
  (lambda (path)
    (define input-string (file->string path))
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this simple-lexer (open-input-string input-string)))
    (let
        ((parser-res (simple-parser my-lexer)))
      
        (car (value-of parser-res (empty-env)))
      )
    )
  )

(evaluate "test.txt")