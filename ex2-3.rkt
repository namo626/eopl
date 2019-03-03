#lang eopl

;; Exercise 2.3
; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
(define (zero) '(diff (one) (one)))
(define (is-zero? n)
  (if (= 0 (reduce n))
      #t
      #f))
(define negative-1 '(diff (diff (one) (one)) (one)))

(define (successor n)
  (list 'diff n negative-1))

(define (predecessor n)
  (list 'diff n '(one)))

(define (diff-tree-plus n m)
  (list 'diff n (list 'diff (zero) m)))


;; Diff-tree -> Int
(define (reduce n)
  (if (eqv? 'one (car n))
      1
      (- (reduce (cadr n))
         (reduce (caddr n)))))
