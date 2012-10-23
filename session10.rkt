#lang planet neil/sicp

; Exercise 2.4
(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

; (car (cons 1 2)) -> (car (lambda (m) (m 1 2))) ->
; ((lambda (m) (m 1 2)) (lambda (p q) p)) ->
; ((lambda (lambda (p q) p) ((lambda (p q) p) 1 2))
; ((lambda (lambda (p q) p) (1))
; 1

; Exercise 2.5
(define (square x) (* x x))
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (iter x result)
    (cond ((= (remainder x 2) 0) (iter (/ x 2) (+ result 1)))
          (else result)))
  (iter x 0))

(define (cdr x)
  (define (iter x result)
    (cond ((= (remainder x 3) 0) (iter (/ x 3) (+ result 1)))
          (else result)))
  (iter x 0))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (x) (f ((zero f) x))))

(define two (lambda (x) (f ((one f) x))))