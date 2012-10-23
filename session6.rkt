#lang planet neil/sicp

; Exercise 1.29
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc a) (+ a 1))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (* (cond ((or (= k 1) (= k n)) 1)
          ((even? k) 2)
          (else 4)) 
       (f (+ a (* h k)))))
  (* (/ h 3) (sum simpson-term 0 inc n)))


; Exercise 1.30

(define (id x) x)

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-term a)
  (cond ((even? a) (/ a (+ a 1)))
        (else (/ (+ a 1) a))))

; example: pi =~ (* 4 (product pi-term 2 inc 10000))

; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
; examples: 
; (accumulate + 0 id 0 inc 10)
; (accumulate * 1 id 1 inc 5)

; Exercise 1.33

(define (prime? n)
  (fast-prime? n 10))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) 
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))
(define (square a) (* a a))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
  
(define (filter-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a null-value))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; example: 
; > (filter-accumulate prime? + 0 square 2 inc 10)
; 87

(define (product-relative-prime n)
  (define (filter x)
    (= (gcd n x) 1))
  (filter-accumulate filter * 1 id 2 inc n))

; example: 
; > (product-relative-prime 11)
; 3628800