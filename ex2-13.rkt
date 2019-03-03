#lang eopl

;; Env = ((Var -> SchemeVal) (_ -> Bool) (Symbol -> Bool))

(define (empty-env)
  (list (lambda (search-var)
          (report-no-binding-found search-var))
        (lambda () #t)
        (lambda (symbol) #f)))

(define (empty-env? env)
  ((cadr env)))

(define (has-binding? env s)
  ((caddr env) s))

(define (extend-env saved-var saved-val saved-env)
  (list (lambda (search-var)
          (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var)))
        (lambda () #f)
        (lambda (symbol)
          (if (eqv? symbol saved-var)
              #t
              (has-binding? saved-env symbol)))))


(define (apply-env env search-var)
  ((car env) search-var))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))

(define e
  (extend-env
   'd 6 (extend-env
         'y 8 (extend-env
               'x 7 (extend-env
                     'y 14 (empty-env))))))
