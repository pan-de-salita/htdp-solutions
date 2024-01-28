#lang racket
(require test-engine/racket-tests)

;; NOTE: logic lifted from S8A:
;;   (https://github.com/S8A/htdp-exercises/blob/master/ex262.rkt)
;; to improve readability for the programmer, slight
;; modifications introduced with additional comments
;; and explanations attempted

;;;; data and constant definitions

;; a Row is one of:
;; - '()
;; - (cons Number Row)
;; i.e. a row of an identity matrix
(define row0 '())
(define row1 (list 1))
(define row2-a (list 1 0))
(define row2-b (list 0 1))
(define row3-a (list 1 0 0))
(define row3-b (list 0 1 0))
(define row3-c (list 0 0 1))

;; a Matrix is one of:
;; - '()
;; - (cons Row Matrix)
;; i.e. the rows making up an identity matrix
(define matrix0 '())
(define matrix1 (list row1))
(define matrix2 (list row2-a row2-b))
(define matrix3 (list row3-a row3-b row3-c))

;;;; functions

;; Number -> Matrix
;; creates an identity materix of size (* n n) with (> n 0)

(check-expect (identity-M 0) matrix0)
(check-expect (identity-M 1) matrix1)
(check-expect (identity-M 2) matrix2)
(check-expect (identity-M 3) matrix3)

(define (identity-M n)
  (cond [(zero? n) '()]
        [else (local (;; 1. create the first Row of the identity matrix
                      ;; Number -> Number
                      (define first-row
                        (build-list n (lambda (x) (if (zero? x) 1 0))))
                      ;; 2. append 0 to an identity matrix of (* (sub1 n) (sub1 n))
                      ;; Matrix -> Matrix
                      (define rest-of-identity-M
                        (map (lambda (x) (cons 0 x)) (identity-M (sub1 n)))))
                (cons first-row rest-of-identity-M))]))

#|

the identity matrix of (* n n) is:
- a top Row computed with (build-list n (lambda (x) (if (zero? x) 1 0)))
- an identity matrix of (* (sub1 n) (sub1 n)) with 0 prepended onto each row

e.g.:

(identity-M 3)
==
| 1 0 0 | -- that is, (build-list 3 (lambda (x) (if (zero? x) 1 0)))
| 0 1 0 | -- that is, (cons 0 (car (identity-M 2)))
| 0 0 1 | -- that is, (cons 0 (cadr (identity-M 2)))

for reference, (identity-M 2)
==
| 1 0 |
| 0 1 |

by prepending 0 to each row, we arrive at the cadr and caddr of (identity-M 3)

|#

;;;; application

(test)
