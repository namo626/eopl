#lang eopl

(provide (all-defined-out))

(define (empty-env)
  (lambda (search-var)
    (report-no-binding-found search-var)))

(define (extend-env saved-var saved-val saved-env)
  (lambda (search-var)
    (if (eqv? search-var saved-var)
        saved-val
        (apply-env saved-env search-var))))

(define (apply-env env search-var)
  (env search-var))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))
