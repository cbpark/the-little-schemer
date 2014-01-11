#lang racket
(require "preface.rkt"
         "chapter04.rkt")

;;; All *-functions work on lists that are either
;;; -- empty,
;;; -- an atom consed onto a list, or
;;; -- a list consed onto a list.

(define rember*
  (lambda (a l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) (cond
                       [(eq? (car l) a) (rember* a (cdr l))]
                       [else (cons (car l)
                                   (rember* a (cdr l)))])]
     [else (cons (rember* a (car l))
                 (rember* a (cdr l)))])))

(module+ test
  (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
  (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))

(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) (cond
                       [(eq? (car l) old)
                        (cons old
                              (cons new
                                    (insertR* new old (cdr l))))]
                       [else (cons (car l)
                                   (insertR* new old (cdr l)))])]
     [else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))])))

(module+ test
  (insertR* 'roast
            'chuck
            '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
              (if (a) ((wood chuck))) could chuck wood)))

;;; The First Commandment (final revision)
;;; When recurring on a list of atoms, lat, ask two questions about it:
;;; (null? lat) and else.
;;; When recurring on a number, n, ask two questions about it: (zero? n) and
;;; else.
;;; When recurring on a list of S-expressions, l, ask three question about it:
;;; (null? l), (atom? (car l)), and else.

;;; The Fourth Commandment (final revision)
;;; Always change at least one argument while recurring. When recurring on a
;;; list of atoms, lat, use (cdr lat). When recurring on a number n, use
;;; (sub1 n). And when recurring on a list of S-expressions, l, use (car l)
;;; and (cdr l) if neither (null? l) nor (atom? (car l)) are true.
;;; It must be changed to be closer to termination. The changing argument must
;;; be tested in the termination condition:
;;; when using cdr, test termination with null? and
;;; when using sub1, test termination with zero?.

(define occur*
  (lambda (a l)
    (cond
     [(null? l) 0]
     [(atom? (car l)) (cond
                       [(eq? (car l) a) (add1 (occur* a (cdr l)))]
                       [else (occur* a (cdr l))])]
     [else (+ (occur* a (car l))
              (occur* a (cdr l)))])))

(module+ test
  (occur* 'banana
          '((banana) (split ((((banana ice))) (cream (banana)) sherbet))
            (banana) (bread) (banana brandy))))

(define subst*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) (cond
                       [(eq? (car l) old) (cons new
                                                (subst* new old (cdr l)))]
                       [else (cons (car l)
                                   (subst* new old (cdr l)))])]
     [else (cons (subst* new old (car l))
                 (subst* new old (cdr l)))])))

(module+ test
  (subst* 'orange 'banana
          '((banana) (split ((((banana ice))) (cream (banana)) sherbet))
            (banana) (bread) (banana brandy))))

(define insertL*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(atom? (car l)) (cond
                       [(eq? (car l) old)
                        (cons new
                              (cons old
                                    (insertL* new old (cdr l))))]
                       [else (cons (car l)
                                   (insertL* new old (cdr l)))])]
     [else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))])))

(module+ test
  (insertL* 'pecker 'chuck
            '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
              (if (a) ((wood chuck))) could chuck wood)))

(define member*
  (lambda (a l)
    (cond
     [(null? l) #f]
     [(atom? (car l)) (or (eq? (car l) a)
                          (member* a (cdr l)))]
     [else (or (member* a (car l))
               (member* a (cdr l)))])))

(module+ test
  (member* 'chips '((potato) (chips ((with) fish) (chips)))))

;; Finds the leftmost atom in a non-empty list of S-expressions that does
;; not contain the empty list.
(define leftmost
  (lambda (l)
    (cond
     [(atom? (car l)) (car l)]
     [else (leftmost (car l))])))

(module+ test
  (leftmost '((potato) (chips ((with) fish) (chips))))
  (leftmost '(((hot) (tuna (and))) cheese)))

(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [(and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
     [(or (atom? (car l1)) (atom? (car l2))) #f]
     [else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))])))

(module+ test
  (eqlist? '(strawberry ice cream) '(strawberry ice cream))
  (eqlist? '(strawberry ice cream) '(strawberry cream ice))
  (eqlist? '(banana ((split))) '((banana) (split)))
  (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))))

(define equal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
     [(or (atom? s1) (atom? s2)) #f]
     [else (eqlist? s1 s2)])))

;;; The Sixth Commandment
;;; Simplify only after the function is correct.

(define rember
  (lambda (s l)
    (cond
     [(null? l) '()]
     [(equal? (car l) s) (cdr l)]
     [else (cons (car l)
                        (rember s (cdr l)))])))
