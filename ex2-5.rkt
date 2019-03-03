#lang eopl

;; using a list to represent an environment
;; Env-exp := () | ((Identifier . Scheme-value) . Env-exp)

(define (empty-env) '())

(define (empty-env? env) (null? env))

(define (extend-env var val env)
  (cons (cons var val) env))

(define (extend-env* vars vals env)
  (let ((bindings (map cons vars vals)))
    (append bindings env)))

(define (has-binding? env s)
  (search-alist env s (lambda () #t) (lambda () #f)))

(define (apply-env env var)
  (search-alist
   env var (lambda (x) x) (lambda () (report-no-binding-found var))))

(define (search-alist alist x found-action notfound-action)
  (cond
    [(null? alist) (notfound-action)]
    [(eqv? (caar alist) x) (found-action (cdar alist))]
    [else
     (search-alist (cdr alist) x found-action notfound-action)]))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))

(define e
  (extend-env
   'd 6 (extend-env
         'y 8 (extend-env
               'x 7 (extend-env
                     'y 14 (empty-env))))))
