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

; Exercise 1.17
(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (** a b)
  (cond ((= b 0) 0)
        ((even? b) (double (** a (halve b))))
        (else (+ a (** a (- b 1))))))

; Exercise 1.18
(define (*** a b)
  (define (iter a b p)
    (cond ((= b 0) p)
          ((even? b) (iter (double a) (halve b) p))
          (else (iter a (- b 1) (+ p a)))))
  (iter a b 0))

; Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
                   