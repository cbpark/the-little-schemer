#lang racket

(provide eqan?
         one?
         pick
         rempick)

;; Treat zero? like null? and add1 like cons.
(define +
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (add1 (+ n (sub1 m)))])))

(define -
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (sub1 (- n (sub1 m)))])))

;;; When building numbers:
;;; the value of the terminal condition -- 0,
;;; the natural terminal condition for a tuple -- (null? tup),
;;; so the terminal condition like looks like  ((null? tup) 0).

;;; The First Commandment (first revision)
;;; When recurring on a list of atoms, lat, ask two questions about it:
;;; (null? lat) and else.
;;; When recurring on a number, n, ask two questions about it:
;;; (zero? n) and else.

(define addtup
  (lambda (tup)
    (cond
     [(null? tup) 0]
     [else (+ (car tup) ; similarly as (cons (car lat) (rember a (cdr lat)))
              (addtup (cdr tup)))])))

;;; The Fourth Commandment (first revision
;;; Always change at least one argument while recurring. It must be changed
;;; to be closer to termination. The changing argument must be tested in the
;;; termination condition:
;;; when using cdr, test termination with null? and
;;; when using sub1, test termination with zero?.

(define *
  (lambda (n m)
    (cond
     [(zero? m) 0]
     [else (+ n
              (* n (sub1 m)))])))

;;; The Fifth Commandment
;;; When building a value with +, always use 0 for the value of the terminating
;;; line, for adding 0 does not change the value of an addition.
;;; When building a value with *, always use 1 for the value of the terminating
;;; line, for multiplying by 1 does not change the value of a multiplication.
;;; When building a value with cons, always consider () for the value of the
;;; terminating line.

(define tup+
  (lambda (tup1 tup2)
    (cond
     [(and (null? tup1) (null? tup2)) '()]
     [else (cons (+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

(module+ test
  (tup+ '(3 7) '(4 6)))

;; tup+ that works for any two tups.
(define tup+-any
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (+ (car tup1) (car tup2))
                 (tup+-any (cdr tup1) (cdr tup2)))])))

(module+ test
  (tup+-any '(3 7) '(4 6 8 1))
  (tup+-any '(3 7 8 1) '(4 6)))

(define >
  (lambda (n m)
    (cond
     [(zero? n) #f]
     [(zero? m) #t]
     [else (> (sub1 n) (sub1 m))])))

(define <
  (lambda (n m)
    (cond
     [(zero? m) #f]
     [(zero? n) #t]
     [else (< (sub1 n) (sub1 m))])))

(define =
  (lambda (n m)
    (cond
     [(> n m) #f]
     [(< n m) #f]
     [else #t])))

(define expt
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (* n (expt n
                      (sub1 m)))])))

(define quotient
  (lambda (n m)
    (cond
     [(< n m) 0]
     [else (add1 (quotient (- n m) m))])))

(define length
  (lambda (lat)
    (cond
     [(null? lat) 0]
     [else (add1 (length (cdr lat)))])))

(define pick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

;; (define rempick
;;   (lambda (n lat)
;;     (cond
;;      [(zero? (sub1 n)) (cdr lat)]
;;      [else (cons (car lat)
;;                  (rempick (sub1 n) (cdr lat)))])))

(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(number? (car lat)) (no-nums (cdr lat))]
            [else (cons (car lat)
                        (no-nums (cdr lat)))])])))

(module+ test
  (no-nums '(5 pears 6 prunes 9 dates)))

(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(number? (car lat)) (cons (car lat)
                                       (all-nums (cdr lat)))]
            [else (all-nums (cdr lat))])])))

(module+ test
  (all-nums '(5 pears 6 prunes 9 dates)))

(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(or (number? a1) (number? a2)) #f]
     [else (eq? a1 a2)])))

;; Counts the number of times an atom a appears in a lat.
(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [else (cond
            [(eq? (car lat) a) (add1 (occur a (cdr lat)))]
            [else (occur a (cdr lat))])])))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     [(one? n) (cdr lat)]
     [else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))])))

(module+ test
  (rempick 3 '(lemon meringue salty pie)))
