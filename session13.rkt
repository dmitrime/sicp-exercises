#lang planet neil/sicp

; Exercise 2.21
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

; Exercise 2.22
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))))))
  (iter items nil))

; Exercise 2.23
(define (for-each f items)
  (cond ((null? items))
        ((f (car items))
         (for-each f (cdr items)))))

(define (for-each2 f items)
  (map f items)
  true)

; Exercise 2.24
;(list 1 (list 2 (list 3 4))) ->
;(mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))

;      (list 1 (list 2 (list 3 4)))
;            1 (list 2 (list 3 4))
;                             2 (list 3 4)
;                                     3 4

; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

; (append x y)
; (mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 '()))))))

; (cons x y)
; (mcons (mcons 1 (mcons 2 (mcons 3 '()))) (mcons 4 (mcons 5 (mcons 6 '()))))

; (list x y)
; (mcons
; (mcons 1 (mcons 2 (mcons 3 '())))
; (mcons (mcons 4 (mcons 5 (mcons 6 '()))) '()))

; Exercise 2.27
(define xx (list (list 1 2) (list 3 4)))

(define (reverse2 list1)
  (define (iter items rev)
    (if (null? items)
        rev
        (iter (cdr items) (cons (car items) rev))))
  (iter list1 (list)))

(define (deep-reverse items)
  (cond ((null? items) nil)
        ((pair? items) 
         (append (deep-reverse (cdr items)) (cons (deep-reverse (car items)) nil)))
        (else items)))

; Exercise 2.28
(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? l)) (list l))
        (else (append (fringe (car l)) (fringe (cdr l))))))
; note to self: using just l in the base case and cons later 
; does not produce a list as desired.

; > (fringe xx)
;(mcons 1 (mcons 2 (mcons 3 (mcons 4 '()))))