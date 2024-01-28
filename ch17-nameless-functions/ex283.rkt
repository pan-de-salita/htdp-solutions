#lang racket
(require test-engine/racket-tests)

;; 1.

(map (lambda (x) (* 10 x)) '(1 2 3))

#|
==
(cons (* 10 1) (map (lambda (x) (* 10 x)) '(2 3)))
==
(cons 10 (map (lambda (x) (* 10 x)) '(2 3)))
==
(cons 10 (cons (* 10 2) (map (lambda (x) (* 10 x)) '(3))))
==
(cons 10 (cons 20 (map (lambda (x) (* 10 x)) '(3))))
==
(cons 10 (cons 20 (cons (* 10 3) (map (lambda (x) (* 10 x)) '()))))
==
(cons 10 (cons 20 (cons 30 (map (lambda (x) (* 10 x)) '()))))
==
(cons 10 (cons 20 (cons 30 '())))
==
'(10 20 30)
|#

(check-expect (map (lambda (x) (* 10 x)) '(1 2 3)) '(10 20 30))

;; 2.

(foldl
 (lambda (name rst)
   (string-append name ", " rst))
 "etc."
 '("Matthew" "Robby"))

#|
==
(foldl
 (lambda (name rst)
   (string-append name ", " rst))
 (string-append "Matthew" ", " "etc.")
 '("Robby"))
==
(foldl
 (lambda (name rst)
   (string-append name ", " rst))
 "Matthew, etc."
 '("Robby"))
==
(foldl
 (lambda (name rst)
   (string-append name ", " rst))
 (string-append "Robby" ", " "Matthew, etc.")
 '())
==
(foldl
 (lambda (name rst)
   (string-append name ", " rst))
 "Robby, Matthew, etc."
 '())
==
"Robby, Matthew, etc."

(check-expect
 (foldl
  (lambda (name rst)
    (string-append name ", " rst))
  "etc."
  '("Robby" "Matthew"))
 "Robby, Matthew, etc.")
|#

;; 3.

(define-struct IR [name price] #:transparent)
(define th 10)

(filter
 (lambda (ir) (<= (IR-price ir) th))
 (list (make-IR "bear" 10)
       (make-IR "doll" 33)))

#|
==
(cond [(empty? (list (make-IR "bear" 10) (make-IR "doll" 33))) '()]
      [else (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "bear" 10))
                (cons (make-IR "bear" 10)
                      (filter
                       (lambda (ir) (<= (IR-price ir) th))
                       (list (make-IR "doll" 33))))
                (filter
                 (lambda (ir) (<= (IR-price ir) th))
                 (list (make-IR "doll" 33))))])
==
(cond [#f '()]
      [else (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "bear" 10))
                (cons (make-IR "bear" 10)
                      (filter
                       (lambda (ir) (<= (IR-price ir) th))
                       (list (make-IR "doll" 33))))
                (filter
                 (lambda (ir) (<= (IR-price ir) th))
                 (list (make-IR "doll" 33))))])
==
(cond [else (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "bear" 10))
                (cons (make-IR "bear" 10)
                      (filter
                       (lambda (ir) (<= (IR-price ir) th))
                       (list (make-IR "doll" 33))))
                (filter
                 (lambda (ir) (<= (IR-price ir) th))
                 (list (make-IR "doll" 33))))])
==
(if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "bear" 10))
    (cons (make-IR "bear" 10)
          (filter
           (lambda (ir) (<= (IR-price ir) th))
           (list (make-IR "doll" 33))))
    (filter
     (lambda (ir) (<= (IR-price ir) th))
     (list (make-IR "doll" 33))))
==
(if (<= (IR-price (make-IR "bear" 10)) th)
    (cons (make-IR "bear" 10)
          (filter
           (lambda (ir) (<= (IR-price ir) th))
           (list (make-IR "doll" 33))))
    (filter
     (lambda (ir) (<= (IR-price ir) th))
     (list (make-IR "doll" 33))))
==
(if (<= (IR-price 10) th)
    (cons (make-IR "bear" 10)
          (filter
           (lambda (ir) (<= (IR-price ir) th))
           (list (make-IR "doll" 33))))
    (filter
     (lambda (ir) (<= (IR-price ir) th))
     (list (make-IR "doll" 33))))
==
(if (<= (IR-price 10) 10)
    (cons (make-IR "bear" 10)
          (filter
           (lambda (ir) (<= (IR-price ir) th))
           (list (make-IR "doll" 33))))
    (filter
     (lambda (ir) (<= (IR-price ir) th))
     (list (make-IR "doll" 33))))
==
(if #t
    (cons (make-IR "bear" 10)
          (filter
           (lambda (ir) (<= (IR-price ir) th))
           (list (make-IR "doll" 33))))
    (filter
     (lambda (ir) (<= (IR-price ir) th))
     (list (make-IR "doll" 33))))
==
(cons (make-IR "bear" 10)
      (filter
       (lambda (ir) (<= (IR-price ir) th))
       (list (make-IR "doll" 33))))
==
(cons (make-IR "bear" 10)
      (cond [(empty? (list (make-IR "doll" 33))) '()]
            [else (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "doll" 33))
                      (cons (make-IR "doll" 33)
                            (filter
                             (lambda (ir (<= (IR-price ir) th)))
                             '()))
                      (filter
                       (lambda (ir (<= (IR-price ir) th)))
                       '()))]))
==
(cons (make-IR "bear" 10)
      (cond [#f '()]
            [else (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "doll" 33))
                      (cons (make-IR "doll" 33)
                            (filter
                             (lambda (ir (<= (IR-price ir) th)))
                             '()))
                      (filter
                       (lambda (ir (<= (IR-price ir) th)))
                       '()))]))
==
(cons (make-IR "bear" 10)
      (cond [else (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "doll" 33))
                      (cons (make-IR "doll" 33)
                            (filter
                             (lambda (ir (<= (IR-price ir) th)))
                             '()))
                      (filter
                       (lambda (ir (<= (IR-price ir) th)))
                       '()))]))
==
(cons (make-IR "bear" 10)
      (if ((lambda (ir) (<= (IR-price ir) th)) (make-IR "doll" 33))
          (cons (make-IR "doll" 33)
                (filter
                 (lambda (ir (<= (IR-price ir) th)))
                 '()))
          (filter
           (lambda (ir (<= (IR-price ir) th)))
           '())))
==
(cons (make-IR "bear" 10)
      (if (<= (IR-price (make-IR "doll" 33)) 10)
          (cons (make-IR "doll" 33)
                (filter
                 (lambda (ir (<= (IR-price ir) th)))
                 '()))
          (filter
           (lambda (ir (<= (IR-price ir) th)))
           '())))
==
(cons (make-IR "bear" 10)
      (if #f
          (cons (make-IR "doll" 33)
                (filter
                 (lambda (ir (<= (IR-price ir) th)))
                 '()))
          (filter
           (lambda (ir (<= (IR-price ir) th)))
           '())))
==
(cons (make-IR "bear" 10)
      (filter
       (lambda (ir (<= (IR-price ir) th)))
       '()))
==
(cons (make-IR "bear" 10)
      (cond [(empty? '()) '()]
            [...]))
==
(cons (make-IR "bear" 10)
      (cond [#t '()]
            [...]))
==
(cons (make-IR "bear" 10) '())
==
(list (make-IR "bear" 10))
|#

;; [X -> Boolean] [List-of X] -> [List-of X]
;; returns a list from those items on l-x for which p holds

(define (own-filter p l-x)
  (cond [(empty? l-x) '()]
        [else (define cdr-filtered (own-filter p (cdr l-x)))
              (if (p (car l-x))
                  (cons (car l-x) cdr-filtered)
                  cdr-filtered)]))

(check-expect
 (filter
  (lambda (ir) (<= (IR-price ir) th))
  (list (make-IR "bear" 10)
        (make-IR "doll" 33)))
 (own-filter
  (lambda (ir) (<= (IR-price ir) th))
  (list (make-IR "bear" 10)
        (make-IR "doll" 33))))

(test)
