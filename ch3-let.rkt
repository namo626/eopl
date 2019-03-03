#lang eopl

(require "environment.rkt")

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
     ("+" "(" expression "," expression ")")
     add-exp)

    (expression
     ("*" "(" expression "," expression ")")
     mult-exp)

    (expression
     ("/" "(" expression "," expression ")")
     div-exp)

    (expression
     ("equal?" "(" expression "," expression ")")
     equal?-exp)

    (expression
     ("greater?" "(" expression "," expression ")")
     greater?-exp)

    (expression
     ("less?" "(" expression "," expression ")")
     less?-exp)

    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)

    (expression
     ("car" "(" expression ")")
     car-exp)

    (expression
     ("cdr" "(" expression ")")
     cdr-exp)

    (expression
     ("emptylist")
     null-exp)

    (expression
     ("null?" "(" expression ")")
     null?-exp)

    (expression
     ("list" "(" (separated-list expression ",") ")")
     list-exp)

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
     ("minus" "(" expression ")")
     minus-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;--------------------------------------------------

(define-datatype expval expval?
  [num-val
   (num number?)]
  [bool-val
   (bool boolean?)]
  [list-val
   (head expval?)
   (tail expval?)]
  [empty-val])

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

;; ExpVal -> List
(define (expval->list val)
  (cases expval val
         [list-val (head tail)
                   (cases expval head
                          [num-val (num) (cons num (expval->list tail))]
                          [bool-val (bool) (cons bool (expval->list tail))]
                          [list-val (h t) (cons (expval->list head) (expval->list tail))]
                          [empty-val () (cons '() (expval->list tail))])]
         [empty-val () '()]

         ;; if not a list, just return the value (can form dotted pair)
         [num-val (num) (expval->num val)]
         [bool-val (bool) (expval->bool val)]

         [else (eopl:error 'expval->list "~s is not an ExpVal!" val)]))

;; ExpVal -> ExpVal
(define (head-val val)
  (cases expval val
         [list-val (head tail) head]
         [else (eopl:error 'head-val "~s is not a list-val" val)]))

(define (tail-val val)
  (cases expval val
         [list-val (head tail) tail]
         [else (eopl:error 'tail-val "~s is not a list-val" val)]))

;; String -> ExpVal
(define (run string)
  (value-of-program (scan&parse string)))

;; Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
         [a-program (exp1)
                    (value-of exp1 (init-env))]))

;; Exp x Env -> ExpVal
(define (value-of exp env)
  (cases expression exp
         [const-exp (num) (num-val num)]
         [var-exp (var) (apply-env env var)]
         [diff-exp (exp1 exp2)
                   (num-op exp1 exp2 - env)]
         ;; ex 3.7
         [add-exp (exp1 exp2)
                  (num-op exp1 exp2 + env)]
         [mult-exp (exp1 exp2)
                   (num-op exp1 exp2 * env)]
         [div-exp (exp1 exp2)
                  (num-op exp1 exp2 quotient env)]

         ;; ex 3.8
         [equal?-exp (exp1 exp2)
                     (exp-binary exp1 exp2 bool-val expval->num = env)]
         [greater?-exp (exp1 exp2)
                       (exp-binary exp1 exp2 bool-val expval->num > env)]
         [less?-exp (exp1 exp2)
                    (exp-binary exp1 exp2 bool-val expval->num < env)]

         ;; ex 3.9
         [cons-exp (exp1 exp2)
                   (let* ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                     (list-val val1 val2))]
         [null-exp ()
                   (empty-val)]
         [car-exp (exp)
                  (head-val (value-of exp env))]
         [cdr-exp (exp)
                  (tail-val (value-of exp env))]
         [null?-exp (exp)
                    (let* ((val (value-of exp env))
                           (ls (expval->list val)))
                      (bool-val (null? ls)))]

         ;; ex 3.10
         [list-exp (exps)
                   (if (null? exps)
                       (empty-val)
                       (list-val (value-of (car exps) env)
                                 (value-of (list-exp (cdr exps)) env)))]


         [zero?-exp (exp1)
                    (let* ((val1 (value-of exp1 env))
                           (num1 (expval->num val1)))
                      (bool-val (zero? num1)))]
         [if-exp (exp1 exp2 exp3)
                 (let ((val1 (value-of exp1 env)))
                   (if (expval->bool val1)
                       (value-of exp2 env)
                       (value-of exp3 env)))]
         [let-exp (var exp1 body)
                  (let ((val1 (value-of exp1 env)))
                    (value-of body
                              (extend-env var val1 env)))]
         [minus-exp (exp1)
                    (let* ((val1 (value-of exp1 env))
                           (num1 (expval->num val1)))
                      (num-val (* -1 num1)))]))


(define (num-op exp1 exp2 proc env)
  (exp-binary exp1 exp2 num-val expval->num proc env))

(define (exp-binary exp1 exp2 wrapper unwrapper proc env)
  (let* ((val1 (value-of exp1 env))
         (val2 (value-of exp2 env))
         (x (unwrapper val1))
         (y (unwrapper val2)))
    (wrapper (proc x y))))

;;; Environment

(define (init-env)
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))

;; tests
(define sample
  "let x = 4 in cons(x,cons(cons(-(x,1),emptylist),emptylist))")
