#lang planet neil/sicp

; Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n td)
  (cond ((> (square td) n ) n)
        ((divides? td n) td)
        (else (find-divisor n (+ td 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square a)
  (* a a))
; (smallest-divisor 199) = 199
; (smallest-divisor 1999) = 1999
; (smallest-divisor 19999) = 7

; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
   (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  ;(= n (smallest-divisor n)))
  (= n (smallest-divisor2 n)))
  ;(fast-prime? n 5))

(define (search-for-primes from to)
  (define (iter cur last)
    (if (<= cur to) (timed-prime-test cur))
    (if (<= cur to) (iter (+ cur 2) to)))
  (iter (if (even? from) (+ from 1))
        (if (even? to) (- to 1))))

; Exercise 1.23
(define (smallest-divisor2 n)
  (find-divisor2 n 2))
(define (find-divisor2 n td)
  (cond ((> (square td) n ) n)
        ((divides? td n) td)
        (else (find-divisor2 n (next td)))))
(define (next d)
  (if (= d 2) 3 (+ d 2)))

; Exercise 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) 
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.25
; Yes, but then fast-exp may return a very large number,
; but with the remainder operator we are dealing with numbers less than m
; Long arithmetic takes longer to compute.