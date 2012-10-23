#lang planet neil/sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Exercise 2.1
(define (make-rat n d)  
  (let ((g (gcd n d)))
    (cond ((< (* n d) 0) (cons (/ (* n -1) g) (/ (* d -1) g)))
          (else (cons (/ n g) (/ d g))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


; Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment xy1 xy2) (cons xy1 xy2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint s)
  (make-point (/ (+ (x-point (start-segment s))
                    (x-point (end-segment s)))
                 2)
              (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s)))
                 2)))

; Exercise 2.3
(define (make-rec p1 p2) (cons p1 p2))
(define (side-a rec) (abs (- (x-point (car rec))
                             (x-point (cdr rec)))))
(define (side-b rec) (abs (- (y-point (car rec))
                             (y-point (cdr rec)))))
(define (perimeter rec) (+ (* 2 (side-a rec))
                           (* 2 (side-b rec))))
(define (area rec) (* (side-a rec) (side-b rec)))

      