#lang eopl

;; Exercise 2.1 - interface for bignum
(define base 16)
(define (zero) '())
(define (is-zero? n) (null? n))
(define (successor n)
  (cond
    ((null? n) '(1))
    ((= (car n) (- base 1))
     (cons 0 (successor (cdr n))))
    (else
     (cons (+ 1 (car n))
           (cdr n)))))

(define (predecessor n)
  (cond
    ((null? n) '())
    ((null? (cdr n))
     (if (= 1 (car n))
         (zero)
         (cons (- (car n) 1) '())))
    ((zero? (car n))
     (cons (- base 1)
           (predecessor (cdr n))))
    (else
     (cons (- (car n) 1)
           (cdr n)))))
