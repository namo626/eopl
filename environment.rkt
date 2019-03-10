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

(define (append-env saved-vars saved-vals saved-env)
  (lambda (search-var)
    (let ((found (search-binding saved-vars
                                 saved-vals
                                 search-var)))
      (if found
          found
          (apply-env saved-env search-var)))))

(define (search-binding xs ys z)
  (cond
    [(not (= (length xs) (length ys)))
     (eopl:error 'search-binding "Unequal lengths of lists")]
    [(null? xs) #f]
    [(eqv? (car xs) z)
     (car ys)]
    [else
     (search-binding (cdr xs) (cdr ys) z)]))


(define (apply-env env search-var)
  (env search-var))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))
