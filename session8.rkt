#lang planet neil/sicp

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))
(define (square x) (* x x))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))
;> (newtons-method (cubic 2 2 2) 10)
;-1.543689012687973

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
;> (((double (double double)) inc) 5)
;21

; Exercise 1.42
(define (compose f g) (lambda (x) (f (g x))))

; Exercise 1.43
(define (repeated func n)
  (define (iter f k)
    (if (<= k 1) f (iter (compose func f) (- k 1))))
  (lambda (x) ((iter func n) x)))
   
;> ((repeated square 2) 5)
;625   

; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (smooth-nfold f n)
  ((repeated smooth n) f))

;> ((smooth-nfold square 15) 2)
;4.000000001

; Exercise 1.45
(define (average x y)
  (/ (+ x y) 2))

; this can be fast-exp as before
(define (pow y n)
  (define (iter k result)
    (if (= k 0) result (iter (- k 1) (* y result))))
  (iter n 1))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log2 n) (/ (log n) (log 2)))

(define (nroot x n)
  (fixed-point ((repeated average-damp (log2 n)) (lambda (y) (/ x (pow y (- n 1)))))
               1.0))

; With one-fold average-damp we can compute 3rd N-root.
; With two-fold average-damp we can compute 7th N-root.
; With two-fold average-damp we can compute 15th N-root.
; It seems that each new average-damp adds the previous amount + 1,
; thus we need to apply log2 of n average-damps.

; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough? guess (improve guess)) guess (iter (improve guess))))
    (iter guess)))

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))
  
(define (sqrt-im x)
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) x))

(define (fixed-point-im f first-guess)
  ((iterative-improve close-enough? f) first-guess))

