#lang planet neil/sicp

(define (f n)
  (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3))))))

(define (g n)
      (g-iter 0 1 2 n))

(define (g-iter a b c n)
  (cond ((= n 0) a)
        (else (g-iter b c (+ c (* 2 b) (* 3 a)) (- n 1)))))