#lang racket

(provide firsts)

;; Rember stands for remove a member
(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a) (cdr lat)]
     [else (cons (car lat)
                 (rember a (cdr lat)))])))

(module+ test
  (rember 'mint '(lamb chops and mint jelly))
  (rember 'mint '(lamb chops and mint flavored min jelly))
  (rember 'toast '(bacon lettuce and tomato))
  (rember 'cup '(coffee cup tea cup and hick cup))
  (rember 'and '(bacon lettuce and tomato))
  (rember 'sauce '(soy sauce and tomato sauce)))

;;; The Second Commandment
;;; Use cons to build lists.

;; The function firsts takes one argument, a list, which is either a null
;; list or contains only non-empty lists. It builds another list composed
;; of the first S-expression of each internall list.
(define firsts
  (lambda (l)
    (cond
     [(null? l) '()]
     [else (cons (car (car l))
                 (firsts (cdr l)))])))

(module+ test
  (firsts '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant)))
  (firsts '((a b) (c d) (e f)))
  (firsts '())
  (firsts '((five plums)
            (four)
            (eleven green oranges)))
  (firsts '(((five plums) four)
            (eleven green oranges)
            ((no) more))))

;;; The Third Commandment
;;; When building a list, describe the first typical element, and then
;;; cons it onto the natural recursion.
;;; Example: (car (car l)): typical element,
;;;          (firsts (cdr l)): natural recursion.

;; The function insertR builds a lat with new inserted to the right of
;; the first occurrence of old.
(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(eq? (car lat) old) (cons old
                                       (cons new
                                             (cdr lat)))]
            [else (cons (car lat)
                        (insertR new old (cdr lat)))])])))

(module+ test
  (insertR 'jalapeno 'and '(tacos tamales and salsa))
  (insertR 'e 'd '(a b c d f g d h)))

(define insertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [else (cond
            ;; [(eq? (car lat) old) (cons new
            ;;                            (cons old
            ;;                                  (cdr lat)))]
            ;; Here, (cons old (cd lat)) equals to (cdr lat).
            [(eq? (car lat) old) (cons new lat)]
            [else (cons (car lat)
                        (insertL new old (cdr lat)))])])))

;; The function subst replaces the first occurrence of old in the lat
;; with new.
(define subst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(eq? (car lat) old) (cons new (cdr lat))]
            [else (cons (car lat)
                        (subst new old (cdr lat)))])])))

(module+ test
  (subst 'topping 'fudge '(ice cream with fudge for dessert)))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     [(null? lat) '()]
     [else (cond
            ;; [(eq? (car lat) o1) (cons new (cdr lat))]
            ;; [(eq? (car lat) o2) (cons new (cdr lat))]
            [(or (eq? (car lat) o1) (eq? (car lat) o2))
             (cons new
                   (cdr lat))]
            [else (cons (car lat)
                        (subst2 new o1 o2 (cdr lat)))])])))

(module+ test
  (subst2 'vanilla 'chocolate 'banana '(banana ice cream
                                               with chocolate topping)))

(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(eq? (car lat) a) (multirember a (cdr lat))]
            [else (cons (car lat)
                        (multirember a (cdr lat)))])])))

(module+ test
  (multirember 'cup '(coffee cup tea cup and hick cup)))

(define multiinsertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [else
      (cond
       [(eq? (car lat) old)
        (cons (car lat)
              (cons new
                    (multiinsertR new old (cdr lat))))]
       [else (cons (car lat)
                   (multiinsertR new old (cdr lat)))])])))

(define multiinsertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [else
      (cond
       [(eq? (car lat) old)
        (cons new
              (cons old
                    (multiinsertL new old (cdr lat))))]
       [else (cons (car lat)
                   (multiinsertL new old (cdr lat)))])])))

(module+ test
  (multiinsertL 'fried 'fish '(chips and fish or fish and fried)))

;;; The Fourth Commandment (preliminary)
;;; Always change at leat one argument while recurring. It must be changed
;;; to be closer to termination. The changing argument must be tested in
;;; the termination condition:
;;; when using cdr, test termination with null?.

(define multisubst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [else
      (cond
       [(eq? (car lat) old)
        (cons new
              (multisubst new old (cdr lat)))]
       [else (cons (car lat)
                   (multisubst new old (cdr lat)))])])))
