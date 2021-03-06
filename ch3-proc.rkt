#lang eopl

(require "environment.rkt")

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;; Parsing -> AST
(define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("letproc" identifier "(" identifier ")" expression "in" expression)
     letproc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))



;; Expressed value in PROC-lang

(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (proc proc?)])

;; ExpVal -> Int
(define (expval->num val)
  (cases expval val
         [num-val (num) num]
         [else (eopl:error 'expval->num "~s is not a num-val" val)]))

;; ExpVal -> Bool
(define (expval->bool val)
  (cases expval val
         [bool-val (bool) bool]
         [else (eopl:error 'expval->bool "~s is not a bool-val" val)]))

(define (expval->proc val)
  (cases expval val
         [proc-val (proc) proc]
         [else (eopl:error 'expval->proc "Expval is not a procedure")]))

;; Representation of procedures in Scheme (not the expressed value)

(define (proc? val)
  (procedure? val))

(define (procedure var body env)
  (lambda (val)
    (value-of body (extend-env var val env))))

(define (apply-procedure proc1 val)
  (proc1 val))


;; The interpreter

(define (run string)
  (value-of-program (scan&parse string)))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           [const-exp (num) (num-val num)]
           [var-exp (var) (apply-env env var)]
           [diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val
                          (- num1 num2))))]
           [zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f))))]
           [if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env)))]
           [let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env)))]
           [proc-exp (var body)
                     (proc-val (procedure var body env))]
           ;; ex 3.19
           [letproc-exp (name var body next)
                        (value-of (let-exp name (proc-exp var body) next) env)]

           [call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg))])))


(define sample
  "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))")

(define sample2
  "let x = 200 in letproc f (z) -(z,x) in let x = 100 in letproc g (z) -(z,x) in -((f 1), (g 1))")

(define curried
  "let f = proc (x) proc (y) -(x, -(0,y)) in ((f 2) 3)")
