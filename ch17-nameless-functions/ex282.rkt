#lang racket
(require test-engine/racket-tests)

;; Number -> Number
;; returns 10 to the power of n

(define (f-plain n) (* 10 n))
(define f-lambda (lambda (n) (* 10 n)))

;; Number -> Boolean
;; compares whether the value of two functions are equal given the same input

(check-random (compare (random 100000)) #t)

(define (compare x)
  (= (f-plain x) (f-lambda x)))

;; [X -> Y] Number X -> Y
;; applies F n times to x

(check-expect ((apply-n-times cdr 0) '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect ((apply-n-times cdr 1) '(1 2 3 4 5)) '(2 3 4 5))
(check-expect ((apply-n-times cdr 2) '(1 2 3 4 5)) '(3 4 5))
(check-expect ((apply-n-times cdr 3) '(1 2 3 4 5)) '(4 5))

(define (apply-n-times F n)
  (lambda (x)
    (cond [(zero? n) x]
          [else (F ((apply-n-times F (sub1 n)) x))])))

;; [X -> Y] Number X -> Y
;; applies F n times to x, just like apply-n-times does

(check-expect (apply-n-times.v2 cdr 0 '(1 2 3 4 5)) ((apply-n-times cdr 0) '(1 2 3 4 5)))
(check-expect (apply-n-times.v2 cdr 1 '(1 2 3 4 5)) ((apply-n-times cdr 1) '(1 2 3 4 5)))
(check-expect (apply-n-times.v2 cdr 2 '(1 2 3 4 5)) ((apply-n-times cdr 2) '(1 2 3 4 5)))
(check-expect (apply-n-times.v2 cdr 3 '(1 2 3 4 5)) ((apply-n-times cdr 3) '(1 2 3 4 5)))

(define apply-n-times.v2
  (lambda (F n x)
    (cond [(zero? n) x]
          [else (apply-n-times.v2 F (sub1 n) (F x))])))

#|
((apply-n-times cdr 2) '(1 2 3 4 5)) -- should return '(3 4 5)
==
(lambda '(1 2 3 4 5)
  (cond [(zero? 2) '(1 2 3 4 5)]
        [else (cdr ((apply-n-times cdr (sub1 2)) '(1 2 3 4 5)))]))
==
(cond [(zero? 2) '(1 2 3 4 5)]
      [else (cdr ((apply-n-times cdr (sub1 2)) '(1 2 3 4 5)))])
==
(cond [#f '(1 2 3 4 5)]
      [else (cdr ((apply-n-times cdr (sub1 2)) '(1 2 3 4 5)))])
==
(cond [else (cdr ((apply-n-times cdr (sub1 2)) '(1 2 3 4 5)))])
==
(cdr ((apply-n-times cdr (sub1 2)) '(1 2 3 4 5)))
==
(cdr ((apply-n-times cdr 1) '(1 2 3 4 5)))
==
(cdr ((lambda '(1 2 3 4 5)
        (cond [(zero? 1) '(1 2 3 4 5)]
              [else (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5)))]))
      '(1 2 3 4 5)))
==
(cdr
 (cond [(zero? 1) '(1 2 3 4 5)]
       [else (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5)))]))
==
(cdr
 (cond [#f '(1 2 3 4 5)]
       [else (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5)))]))
==
(cdr
 (cond [else (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5)))]))
==
(cdr (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5))))
==
(cdr
 (cdr
  ((apply-n-times cdr 0) '(1 2 3 4 5))))
==
(cdr
 (cdr
  (cond [(zero? 0) '(1 2 3 4 5)]
        [else (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5)))])))
==
(cdr
 (cdr
  (cond [#t '(1 2 3 4 5)]
        [else (cdr ((apply-n-times cdr (sub1 1)) '(1 2 3 4 5)))])))
==
(cdr (cdr '(1 2 3 4 5)))
==
(cdr '(2 3 4 5))
==
'(3 4 5)
|#


(test)
