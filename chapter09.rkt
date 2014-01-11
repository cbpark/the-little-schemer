#lang racket
(require "preface.rkt"
         "chapter04.rkt"
         "chapter07.rkt")

;; Unnatural recursion.
;; It does not recur on a part of lat.
(define keep-looking
  (lambda (a sorn lat)
    (cond
     [(number? sorn) (keep-looking a (pick sorn lat) lat)]
     [else (eq? sorn a)])))

;; A Partial function. (<-> total functions.)
;; (looking 'caviar '(7 1 2 caviar 5 6 3) will keep looking forever.
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;; An example of the most partial function.
(define eternity
  (lambda (x)
    (eternity x)))

;; The function shift takes a pair whose first component is a pair and builds
;; a pair by shifting the second part of the first component into the second
;; component.
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

;; A total function.
(define align
  (lambda (pora)
    (cond
     [(atom? pora) pora]
     [(pair? (first pora)) (align (shift pora))]
     [else (build (first pora)
                  (align (second pora)))])))

(define length*
  (lambda (pora)
    (cond
     [(atom? pora) 1]
     [else (+ (length* (first pora))
              (length* (second pora)))])))

(define weight*
  (lambda (pora)
    (cond
     [(atom? pora) 1]
     [else (+ (* (weight* (first pora)) 2)
              (weight* (second pora)))])))

;; A partial function.
(define shuffle
  (lambda (pora)
    (cond
     [(atom? pora) pora]
     [(pair? (first pora)) (shuffle (revpair pora))]
     [else (build (first pora)
                  (shuffle (second pora)))])))

;; A partial function. Consider (C 0)
(define C
  (lambda (n)
    (cond
     [(one? n) 1]
     [else (cond
            [(even? n) (C (/ n 2))]
            [else (C (add1 (* 3 n)))])])))

;; A partial function. Consider (A 4 3)
(define A
  (lambda (n m)
    (cond
     [(zero? n) (add1 m)]
     [(zero? m) (A (sub1 n) 1)]
     [else (A (sub1 n)
              (A n (sub1 m)))])))

;;; Y combinator:
;;; The Y combinator is a higher-order function. It takes a single
;;; argument, which is a function that is not recursive. It returns
;;; a version of the function which is recursive.

;; length-0 -- can only determine the length of the empty list.
(lambda (l)
  (cond
   [(null? l) 0]
   [else
    (add1 (eternity
           (cdr l)))]))

;; length-1 -- can only determine the length of lists that contain
;; one or fewer items.
(lambda (l)
  (cond
   [(null? l) 0]
   [else
    (add1 ((lambda (l)
             (cond
              [(null? l) 0]
              [else
               (add1 (eternity
                      (cdr l)))]))
           (cdr l)))]))

;; length-2 -- can only determine the length of lists that contain
;; two or fewer items.
(lambda (l)
  (cond
   [(null? l) 0]
   [else
    (add1 ((lambda (l)
             (cond
              [(null? l) 0]
              [else
               (add1 ((lambda (l)
                        (cond
                         [(null? l) 0]
                         [else
                          (add1 (eternity
                                 (cdr l)))]))
                      (cdr l)))]))
           (cdr l)))]))

;; Abstract out a function looks like length.
;; length-0
((lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
 eternity)

;; length-1
((lambda (f)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (f (cdr l)))])))
 ((lambda (g)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (g (cdr l)))])))
  eternity))

;; length-2
((lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
 ((lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))
  ((lambda (length)
     (lambda (l)
       (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))])))
   eternity)))

;; Make length -- takes length as an argument and returns a function that
;; looks like length
;; length-0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;; length-1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;; length-2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;; length-3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;; Pass (mk-length eternity) to mk-length.
;; length-0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1
             (length (cdr l)))]))))

;; mk-length instead of length in length-0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1
             (mk-length (cdr l)))]))))

;; length-1
((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
        [(null? l) 0]
        [else (add1
               ((mk-length eternity)
                (cdr l)))]))))

;; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1
             ((mk-length mk-length)
              (cdr l)))]))))

;; (mk-length mk-length) returns a function of one argument.
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (lambda (x)
                    ((mk-length mk-length) x))
                  (cdr l))]))))

;; Move out (mk-length mk-length) as a function.
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         [(null? l) 0]
         [else (add1 length (cdr l))])))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; Extract the length function.
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;; Separate teh functions.
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

;; Applicative-order Y combinator.
(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x)
             ((f f) x)))))))
