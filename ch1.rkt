#lang eopl

;; remove-first : Sym x Listof(Sym) -> Listof(Sym)
(define (remove-first s los)
  (if (null? los)
      '()
      (if (eqv? (car los) s)
          (cdr los)
          (cons (car los) (remove-first s (cdr los))))))


;; Exercise 1.9
(define (remove s los)
  (if (null? los)
      '()
      (if (eqv? (car los) s)
          (remove s (cdr los))
          (cons (car los) (remove s (cdr los))))))

;; Exercise 1.13
;; S-list ::= ({S-exp}*)
;; S-exp  ::= Symbol | S-list

(define (subst new old slist)
  (map (lambda (sexp)
         (subst-in-s-exp new old sexp))
       slist))

(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
      (subst new old sexp)))
