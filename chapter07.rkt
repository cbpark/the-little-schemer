#lang racket
(require "preface.rkt"
         "chapter03.rkt")

(provide first
         second
         build
         third
         revpair)

(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [else (or (equal? (car lat) a)
               (member? a (cdr lat)))])))

(define set?
  (lambda (lat)
    (cond
     [(null? lat) #t]
     [(member? (car lat) (cdr lat)) #f]
            [else (set? (cdr lat))])))

(module+ test
  (set? '(apple peaches apple plum))
  (set? '(apples peaches pears plums))
  (set? '(apple 3 pear 4 9 apple 3 4)))

(define makeset
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
     [else (cons (car lat)
                 (makeset (cdr lat)))])))

(module+ test
  (makeset '(apple peach pear peach plum apple lemon peach)))

(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [else (cond
            [(equal? (car lat) a) (multirember a (cdr lat))]
            [else (cons (car lat)
                        (multirember a (cdr lat)))])])))

;; Rembers to cons the first atom in the lat onto the result of the natural
;; recursion, after removing all occurrences of the first atom from the rest
;; of the lat.
(define makeset-second
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else (cons (car lat)
                 (makeset (multirember (car lat) (cdr lat))))])))

(module+ test
  (makeset-second '(apple peach pear peach plum apple lemon peach))
  (makeset-second '(apple 3 pear 4 9 apple 3 4)))

(define subset?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [(member? (car set1) set2) (subset? (cdr set1) set2)]
     [else #f])))

(module+ test
  (subset? '(5 chicken wings)
           '(5 hamburgers
               2 pieces fried chicken and
               light duckling wings))
  (subset? '(4 pounds of horseradish)
           '(four pounds chicken and 5 ounces horseradish)))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(module+ test
  (eqset? '(6 large chickens with wings)
          '(6 chickens with large wings)))

(define intersect?
  (lambda (set1 set2)
    (cond
     [(null? set1) #f]
     [else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))])))

(module+ test
  (intersect? '(stewed tomatoes and macaroni)
              '(macaroni and cheese)))

(define intersect
  (lambda (set1 set2)
    (cond
     [(null? set1) '()]
     [(member? (car set1) set2) (cons (car set1)
                                      (intersect (cdr set1) set2))]
     [else (intersect (cdr set1) set2)])))

(module+ test
  (intersect '(stewed tomatoes and macaroni)
             '(macaroni and cheese)))

(define union
  (lambda (set1 set2)
    (cond
     [(null? set1) set2]
     [(member? (car set1) set2) (union (cdr set1) set2)]
     [else (cons (car set1)
                 (union (cdr set1) set2))])))

(module+ test
  (union '(stewed tomatoes and macaroni casserole)
         '(macaroni and cheese)))

(define intersectall
  (lambda (l-set)
    (cond
     [(null? (cdr l-set)) (car l-set)]
     [else (intersect (car l-set)
                      (intersectall (cdr l-set)))])))

(module+ test
  (intersectall '((a b c) (c a d e) (e f g h a b)))
  (intersectall '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with some apples))))

(define a-pair?
  (lambda (x)
    (cond
     [(atom? x) #f]
     [(null? x) #f]
     [(null? (cdr x)) #f]
     [(null? (cdr (cdr x))) #t]
     [else #f])))

(module+ test
  (a-pair? '(pear pear))
  (a-pair? '(3 7))
  (a-pair? '((2) (pair)))
  (a-pair? '(full (house))))

(define first
  (lambda (p)
    (cond
     (else (car p)))))

(define second
  (lambda (p)
    (cond
     (else (car (cdr p))))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2
                '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(module+ test
  (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
  (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     [(null? rel) '()]
     [else (cons (revpair (car rel))
                 (revrel (cdr rel)))])))

(module+ test
  (revrel '((8 a) (pumpkin pie) (got sick))))

(define seconds
  (lambda (l)
    (cond
     [(null? l) '()]
     [else (cons (car (cdr (car l)))
                 (seconds (cdr l)))])))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(module+ test
  (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
  (fullfun? '((grape raisin) (plum prune) (stewed prune)))
  (fullfun? '((grape raisin) (plum prune) (stewed grape))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(module+ test
  (one-to-one? '((chocolate chip) (doughy cookie))))
