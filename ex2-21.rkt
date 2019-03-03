#lang eopl

;; Env = (empty-env) | (extend-env Var SchemeVal Env)

(define (id x) x)

(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
   (var symbol?)
   (val id)
   (saved-env env-exp?)))

(define (apply-env env s)
  (cases env-exp env
         (empty-env
          ()
          (report-no-binding-found s))
         (extend-env
          (var val saved-env)
          (if (eqv? s var)
              val
              (apply-env saved-env s)))))


(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))

(define e
  (extend-env
   'd 6 (extend-env
         'y 8 (extend-env
               'x 7 (extend-env
                     'y 14 (empty-env))))))
