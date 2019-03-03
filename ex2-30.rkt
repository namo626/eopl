#lang eopl

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? 'lambda x))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define (parse-expression datum)
  (cond
    [(symbol? datum) (var-exp datum)]
    [(list? datum)
     (let ((len (length datum)))
       (cond
         [(and (= len 3) (eqv? 'lambda (car datum)))
          (lambda-exp (caadr datum) (parse-expression (caddr datum)))]
         [(= len 2)
          (app-exp (parse-expression (car datum))
                   (parse-expression (cadr datum)))]
         [(eopl:error 'parse-expression "Wrong expression for lambda/application")]))]
    [else
     (eopl:error 'parse-expression "Expression is neither a symbol nor list")]))
