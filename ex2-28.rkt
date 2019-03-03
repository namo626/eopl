#lang eopl

(define (identifier? x)
  (and (symbol? x)
       (not (eqv? x 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; unparse-lc-exp : lc-exp -> String
(define (unparse-lc-exp expr)
  (cases lc-exp expr
         [var-exp (var) (symbol->string var)]
         [lambda-exp (bound-var body)
                     (string-append "(lambda ("
                                    (symbol->string bound-var)
                                    ") "
                                    (unparse-lc-exp body)
                                    ")")]
         [app-exp (rator rand)
                  (string-append "("
                                 (unparse-lc-exp rator)
                                 " "
                                 (unparse-lc-exp rand)
                                 ")")]))

(define e
  (app-exp (lambda-exp 'a (app-exp (var-exp 'a) (var-exp 'b)))
           (var-exp 'c)))
