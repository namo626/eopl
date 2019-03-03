#lang eopl

;; Env-exp := () | (Rib . Env-exp)
;; Rib     := (list-of-symbols . list-of-values)

(define (empty-env) '())

(define (extend-env* vars vals env)
  (cons (cons vars vals) env))

(define (apply-env env var)
  (cond
    [(null? env)
     (report-no-binding-found var)]
    [else
     (let ((result (search-rib (car env) var)))
       (if result
           result
           (apply-env (cdr env) var)))]))

(define (search-rib rib s)
  (let ((syms (car rib))
        (vals (cdr rib)))
  (cond
    [(null? syms) #f]
    [(eqv? s (car syms))
     (car vals)]
    [else
     (search-rib (cons (cdr syms) (cdr vals)) s)])))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))

(define e
  (extend-env* '(a b c) '(11 12 13)
               (extend-env* '(x z) '(66 77)
                            (extend-env* '(x y) '(88 99)
                                         (empty-env)))))
