#lang planet neil/sicp

; Exercise 1.26
; beause calling (* (expmod ...) (expmod ...))
; takes O(2^(log n)) ~ O(n) steps since expmod takes O(log n)
; and the multiplication is a tree recursion.

; Exercise 1.27
; 561, 1105, 1729, 2465, 2821, and 6601.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (square x)
  (* x x))

(define (fermat-test n)
  (define (iter a n)
    (if (>= a n) 
        true
        (if (= (expmod a n n) a)
            (iter (+ a 1) n)
            false)))
  (iter 1 n))


; Exercise 1.28
(define (mr-expmod base exp m)
  (define (square-expmod x)
    (define (check x sq)
      (if (and (= sq 1)
               (= x 1)
               (= x (- m 1)))
          0
          sq))
    (check x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-expmod (mr-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (mr-expmod base (- exp 1) m))
                    m))))


(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (mr-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))
  
(define (prime? n)
  (define (iter n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (iter n (- times 1)))
          (else false)))
  (iter n 100))
