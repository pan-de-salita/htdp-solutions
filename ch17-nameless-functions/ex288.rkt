#lang racket
(require test-engine/racket-tests)

;; a NaturalNumber is any Integer greater than 0

;; functions ------------------------------

;; NaturalNumber -> [List-of Number]
;; creates the list (list 0 ... (- n 1)) for any natural number n

(check-expect (sub1/list 1) '(0))
(check-expect (sub1/list 2) '(0 1))
(check-expect (sub1/list 3) '(0 1 2))

(define (sub1/list n)
  (build-list n (lambda (x) x)))

;; NaturalNumber -> [List-of Number]
;; creates the list (list 1 ... n) for any natural number n

(check-expect (add1/list 1) '(1))
(check-expect (add1/list 2) '(1 2))
(check-expect (add1/list 3) '(1 2 3))

(define (add1/list n)
  (build-list n (lambda (x) (add1 x))))

;; NaturalNumber -> [List-of Number]
;; creates the list (list 1 1/2 ... 1/n) for any natural number n

(check-expect (n-as-denominator 1) '(1))
(check-expect (n-as-denominator 2) '(1 1/2))
(check-expect (n-as-denominator 3) '(1 1/2 1/3))

(define (n-as-denominator n)
  (build-list n (lambda (x) (/ 1 (add1 x)))))

;; NaturalNumber -> [List-of Number]
;; creates the list of the first n even numbers

(check-expect (first-n-even-numbers 1) '(0))
(check-expect (first-n-even-numbers 2) '(0 2))
(check-expect (first-n-even-numbers 3) '(0 2 4))

(define (first-n-even-numbers n)
  (build-list n (lambda (x) (* x 2))))

;; a MatrixRow is a [List-of Number] with:
;; - exactly one 1 and
;; - zero or more 0s
;; examples:
;; - '(1)
;; - '(1 0)
;; - '(0 1)
;; - '(1 0 0)
;; - '(0 1 0)
;; - '(0 0 1)

;; an IdentityMatrix is a [NE-List-of Row]
;; examples:
;; - (list '(1))
;; - (list '(1 0)
;;         '(0 1))
;; - (list '(1 0 0)
;;         '(0 1 0)
;;         '(0 0 1))

;; NaturalNumber -> IdentityMatrix
;; creates an IdentityMatrix of size (sqr n)

(check-expect (identity-matrix 1) '((1)))
(check-expect
 (identity-matrix 2)
 '((1 0)
   (0 1)))
(check-expect
 (identity-matrix 3)
 '((1 0 0)
   (0 1 0)
   (0 0 1)))

(define (identity-matrix n)
  (build-list ;; builds resulting IdentityMatrix
   n ;; determines number of MatrixRows
   (lambda (posn-of-1-on-row-n)
     (build-list ;; builds each MatrixRow
      n ;; determines length of resulting MatrixRow
      (lambda (digit-index)
        (if (= digit-index posn-of-1-on-row-n) 1 0))))))

;; NaturalNumber -> IdentityMatrix
;; like identity-matrix, creates an IdentityMatrix of size (sqr n)

(check-expect (identity-matrix.v2 1) (identity-matrix 1))
(check-expect (identity-matrix.v2 2) (identity-matrix 2))
(check-expect (identity-matrix.v2 3) (identity-matrix 3))

(define (identity-matrix.v2 n)
  (cond [(zero? n) '()]
        [else (cons (build-list n (lambda (digit-index) (if (= digit-index 0) 1 0)))
                    (map (lambda (matrix-row) (cons 0 matrix-row))
                         (identity-matrix.v2 (sub1 n))))]))

;; Number [Number -> Number] -> [List-of Number]
;; tabulates F between n and 0 (incl.) in a list

(check-expect (tabulate 0 add1) '(1))
(check-expect (tabulate 1 sub1) '(0 -1))
(check-expect (tabulate 2 sqr) '(4 1 0))
(check-expect (tabulate 3 (lambda (x) (* x 10))) '(30 20 10 0))

(define (tabulate n F)
  (reverse (build-list (add1 n) F)))

;;;; application --------------------------

(test)
