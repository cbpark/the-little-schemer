#lang racket
(require "preface.rkt")

(define rember-f
  (lambda (test? a l)
    (cond
     [(null? l) '()]
     [(test? (car l) a) (cdr l)]
     [else (cons (car l)
                 (rember-f test? a (cdr l)))])))

(module+ test
  (rember-f = 5 '(6 2 5 3))
  (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))))

;;; Currying:
;;; (lambda (a) (lambda (x) (eq? x a))) is a funciton that, when passed an argument
;;; a, returns the function (lambda (x) (eq? x a)) where a is just that argument.

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(module+ test
  (eq?-salad 'salad)
  (eq?-salad 'tuna))

(define rember-f-second
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) '()]
       [(test? (car l) a) (cdr l)]
       [else (cons (car l)
                   ((rember-f-second test?) a (cdr l)))]))))

(define rember-eq? (rember-f-second eq?))

(module+ test
  (rember-eq? 'tuna '(tuna salad is good))
  ((rember-f-second eq?) 'tuna '(shrimp salad and tuna salad))
  ((rember-f-second eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) '()]
       [(test? (car l) old) (cons new
                                  (cons old
                                        (cdr l)))]
       [else (cons (car l)
                   ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) '()]
       [(test? (car l) old) (cons old
                                  (cons new
                                        (cdr l)))]
       [else (cons (car l)
                   ((insertR-f test?) new old (cdr l)))]))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       [(null? l) '()]
       [(eq? (car l) old) (seq new old (cdr l))]
       [else (cons (car l)
                   ((insert-g seq) new old (cdr l)))]))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(module+ test
  (rember 'sausage '(pizza with sausage and bacon)))

;;; The Ninth Commandment
;;; Abstract common patterns with a new function.

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       [(null? lat) '()]
       [(test? a (car lat)) ((multirember-f test?) a (cdr lat))]
       [else (cons (car lat)
                   ((multirember-f test?) a (cdr lat)))]))))

(module+ test
  ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     [(null? lat) '()]
     [(test? (car lat)) (multiremberT test? (cdr lat))]
     [else (cons (car lat)
                 (multiremberT test? (cdr lat)))])))

(module+ test
  (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))

;; It looks at every atom of the lat to see whether it is eq? to a.
;; Those atoms that are not collect in on list ls1; thhe other for which
;; the answer is true are collected in a second ls2. Finally, it determins
;; the value of (f ls1 ls2).
(define multirember&co
  (lambda (a lat col)
    (cond
     [(null? lat) (col '() '())]
     [(eq? (car lat) a)
      (multirember&co a (cdr lat) (lambda (newlat seen)
                                    (col newlat (cons (car lat)
                                                      seen))))]
     [else (multirember&co a (cdr lat) (lambda (newlat seen)
                                         (col (cons (car lat)
                                                    newlat)
                                              seen)))])))

(define a-friend
  (lambda (x y)
    (null? y)))

(module+ test
  (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
  (multirember&co 'tuna '() a-friend)
  (multirember&co 'tuna '(tuna) a-friend))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen)))) ; -> #f

(module+ test
  (multirember&co 'tuna '(and tuna) a-friend))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen))) ; -> #f

(define last-friend
  (lambda (x y)
    (length x)))

(module+ test
  (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))

;;; The Tenth Commandment
;;; Build functions to collect more than one value at a time.

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) oldL)
      (cons oldL
            (multiinsertLR new oldL oldR (cdr lat)))]
     [(eq? (car lat) oldR)
      (cons oldR
            (multiinsertLR new oldL oldR (cdr lat)))]
     [else (cons (car lat)
                 (multiinsertLR new oldL oldR (cdr lat)))])))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ;; 0 occurrence of oldL and oldR is found.
     ;; multiinsertLR returns () when lat is empty.
     [(null? lat) (col '() 0 0)]
     [(eq? (car lat) oldL)
      (cons oldL
            (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col (cons new
                                           (cons oldL newlat))
                                     (add1 L) R))))]
     [(eq? (car lat) oldR)
      (cons oldR
            (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col (cons oldR
                                           (cons new newlat))
                                     L (add1 R)))))]
     [else (cons (car lat)
                 (multiinsertLR&co new oldL oldR (cdr lat)
                                   (lambda (newlat L R)
                                     (col (cons (car lat)
                                                newlat)
                                          L R))))])))

(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) (cond
                       [(even? (car l)) (cons (car l)
                                              (evens-only* (cdr l)))]
                       [else (evens-only* (cdr l))])]
     [else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))])))

(module+ test
  (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

(define evens-only*&co
  (lambda (l col)
    (cond
     [(null? l) (col '() 1 0)]
     [(atom? (car l)) (cond
                       [(even? (car l))
                        (evens-only*&co (cdr l)
                                        (lambda (newl p s)
                                          (col (cons (car l)
                                                     newl)
                                               (* (car l) p) s)))]
                       [else (evens-only*&co (cdr l)
                                             (lambda (newl p s)
                                               (col newl
                                                    p (+ (car l) s))))])]
     [else (evens-only*&co (car l) (lambda (al ap as)
                                     (evens-only*&co (cdr l)
                                                     (lambda (dl dp ds)
                                                       (col (cons al dl)
                                                            (* ap dp)
                                                            (+ as ds))))))])))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

(module+ test
  (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend))
