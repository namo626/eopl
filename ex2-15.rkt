#lang eopl

;; Lc-exp ::= Identifier
;;        ::= (lambda (Identifier) Lc-exp)
;;        ::= (Lc-exp Lc-exp)

;; Constructors
(define (var-exp var) var)

(define (lambda-exp var expr)
  (list 'lambda (list var) expr))

(define (app-exp expr1 expr2)
  (cons expr1 expr2))

;; Predicates
(define (var-exp? expr)
  (symbol? expr))

(define (lambda-exp? expr)
  (if (var-exp? expr)
      #f
      (eqv? 'lambda (car expr))))

(define (app-exp? expr)
  (and (not (var-exp? expr))
       (not (lambda-exp? expr))))

;; Extractors
(define (var-exp->var expr) expr)

(define (lambda-exp->bound-var expr)
  (caadr expr))

(define (lambda-exp->body expr)
  (caddr expr))

(define (app-exp->rator expr)
  (car expr))

(define (app-exp->rand expr)
  (cdr expr))
