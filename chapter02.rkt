#lang racket
(require "preface.rkt")

;; A lat is a list of atoms.
(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [(atom? (car l)) (lat? (cdr l))]
     [else #f])))

(module+ test
  (lat? '(bacon and eggs)))

(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [else (or (eq? (car lat) a)
               (member? a (cdr lat)))])))

(module+ test
  (member? 'meat '(mashed potatoes and meat gravy)))

;;; The First Commandment (preliminary)
;;; Always ask null? as the first question in expressing any function.
