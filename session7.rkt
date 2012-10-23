#lang planet neil/sicp

; Exercise 1.34
(define (f g)
  (g 2))

(define (square x)
  (* x x))

; > (f f)
; procedure application: expected procedure, given: 2; arguments were: 2
; First we call (f f) this calls (f 2) inside the f body. Now, it tries to
; do (2 2) which makes no sence, so we get an error.


; Exercise 1.35
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;> (golden-ratio)
;1.6180327868852458

; Exercise 1.36
(define (fixed-point-verbose f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;> (fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 2.0)
; 30 lines later (with tolerance 0.0001)
;4.555563237292884

;> (fixed-point-verbose (lambda (x) (average x (/ (log 1000) (log x)))) 2.0
; 8 lines later (with tolerance 0.0001)
;4.5555465521473675

; Thus, using average damping helps to find an approximate solution faster!


; Exercise 1.37
(define (cont-frac n d k)
  (define (iter kk result)
    (cond ((= kk 0) result)
          (else (iter (- kk 1) (/ (n kk) (+ (d kk) result))))))
  (iter k 0))

;> (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12))
; 1.6180555555555558

; Exercise 1.38
(define (euler)
  (define (Ni n) 1.0)
  (define (Di n)
    (if (= n 0) 1
        (let
             ((k (- n 1)))
           (if (= (remainder k 3) 1) (+ 2 (* 2 (quotient k 3))) 1))))
  (cont-frac Ni Di 10))

;> (+ 2 (euler))
;2.7182817182817183


; Exercise 1.39
(define (tan-cf x k)
  (define (iter kk result)
    (cond ((= kk 0) (/ x (- 1 result)))
          (else (iter (- kk 1) (/ (* x x) (- (+ 1 (* 2 kk)) result))))))
  (iter k 0))

;> (tan-cf 0.785 10)
;0.9992039901050428

(define (tan-cont-frac x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))
;> (tan-cont-frac 0.785 2)
;0.9879288103952764