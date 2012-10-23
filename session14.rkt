#lang planet neil/sicp

; Exercise 2.29
(define (last-pair list1)
  (define (iter items last)
    (if (null? items)
        last
        (iter (cdr items) (car items))))
  (if (null? list1) nil (iter list1 (car list1))))

(define (make-mobile left right)
  (list left right))

(define (left-branch mob) (car mob))
(define (right-branch mob) (last-pair mob)) ; can also use (car (cdr mob))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (last-pair branch)) ; can also use (car (cdr mob))

(define mobl (make-mobile 
              (make-branch 1 (make-mobile 
                              (make-branch 1 10)
                              (make-branch 1 10)))
              (make-branch 1 (make-mobile
                              (make-branch 1 10)
                              (make-branch 1 10)))))

(define (left-structure mobile) (branch-structure (left-branch mobile)))
(define (right-structure mobile) (branch-structure (right-branch mobile)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (left-structure mobile))
                 (total-weight (right-structure mobile))))))

(define (left-length mobile) (branch-length (left-branch mobile)))
(define (right-length mobile) (branch-length (right-branch mobile)))

(define (balanced-branch? br)
  (if (pair? (branch-structure br)) (balanced? (branch-structure br)) true))

(define (torque bt) (* (branch-length bt) (total-weight (branch-structure bt))))
                     
(define (balanced? mob)
  (and (= (torque (left-branch mob)) (torque (right-branch mob)))
       (balanced-branch? (left-branch mob))
       (balanced-branch? (right-branch mob))))
        
