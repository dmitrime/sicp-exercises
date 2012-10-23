#lang planet neil/sicp

; Exercise 1.12

(define (tri r c)
  (cond ((= r 0) 1)
        ((= c 0) 1)
        ((= c r) 1)
        (else (+ (tri (- r 1) c)
                 (tri (- r 1) (- c 1))))))

; Exercise 1.16
(define (exp b n)
  (exp-iter b n 1))

(define (exp-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (exp-iter (* b b) (/ n 2) a))
        (else (exp-iter b (- n 1) (* b a)))))