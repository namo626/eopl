#lang eopl

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? 'lambda x))))

(define-datatype lc-exp lc-exp?
  [var-exp
   (var identifier?)]
  [lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))])

;; SchemeVal -> LcExp
(define (parse-exp datum)
  (cond
    [(symbol? datum)
     (var-exp datum)]
    [(pair? datum)
     (if (eqv? (car datum) 'lambda)
         (lambda-exp (caadr datum)
                     (parse-exp (caddr datum)))
         (app-exp (parse-exp (car datum))
                  (map parse-exp (cadr datum))))]))

(define (list-of pred)
  (lambda (ls)
    (cond
      [(not (list? ls)) #f]
      [(null? ls) #t]
      [(pred (car ls)) ((list-of pred) (cdr ls))]
      [else #f])))
