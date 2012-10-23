#lang planet neil/sicp

; Exercise 2.17
(define (last-pair list1)
  (define (iter items last)
    (if (null? items)
        last
        (iter (cdr items) (car items))))
  (if (null? list1) nil (iter list1 (car list1))))

; Exercise 2.18
(define (reverse2 list1)
  (define (iter items rev)
    (if (null? items)
        rev
        (iter (cdr items) (cons (car items) rev))))
  (iter list1 (list)))

; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount)
  (cc amount us-coins))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(define (first-denomination values) (car values))
(define (except-first values) (cdr values))
(define (no-more? values) (null? values))

; Orver of coin-values does not affect the answer because it's a tree
; recursion and all possibilities are considered.

; Exercise 2.20
(define (same-parity a . vec)
  (define rem (remainder a 2))
  (define (iter vec)
    (if (null? vec)
        nil
        (if (= rem (remainder (car vec) 2))
            (cons (car vec) (iter (cdr vec)))
            (iter (cdr vec)))))
  (cons a (iter vec)))
            