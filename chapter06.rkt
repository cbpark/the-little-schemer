#lang racket
(require "preface.rkt")

(define numbered?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [else (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))])))

(module+ test
  (numbered? 1)
  (numbered? '(1 + 3))
  (numbered? '(n + 3)))

(define value
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(eq? (car (cdr nexp)) '+)
      (+ (value (car nexp))
         (value (car (cdr (cdr nexp)))))]
     [(eq? (car (cdr nexp)) '*)
      (* (value (car nexp))
         (value (car (cdr (cdr nexp)))))]
     [(eq? (car (cdr nexp)) 'expt)
      (expt (value (car nexp))
            (value (car (cdr (cdr nexp)))))])))

(module+ test
  (value 13)
  (value '(1 + 3))
  (value '(1 + (3 expt 4))))

;;; The Seventh Commandment
;;; Recur on the subparts that are of the same nature:
;;; - On the sublists of a list.
;;; - On the subexpressions of an arithmetic expression.

;; Help functions.
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-second
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(eq? (operator nexp) '+)
      (+ (value-second (1st-sub-exp nexp))
         (value-second (2nd-sub-exp nexp)))]
     [(eq? (operator nexp) '*)
      (* (value-second (1st-sub-exp nexp))
         (value-second (2nd-sub-exp nexp)))]
     [(eq? (operator nexp) 'expt)
      (expt (value-second (1st-sub-exp nexp))
            (value-second (2nd-sub-exp nexp)))])))

(module+ test
  (value-second 13)
  (value-second '(+ 1 3)))

;;; The Eighth Commandment
;;; Use help functions to abstract from representations.
