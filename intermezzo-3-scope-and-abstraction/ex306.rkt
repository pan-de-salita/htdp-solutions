#lang racket
(require test-engine/racket-tests)

;; Number -> [List-of Number]
;; creates the list '(0 ... ,(- n 1)) for any natural number n

(check-expect (for/build-list 0) '())
(check-expect (for/build-list 1) '(0))
(check-expect (for/build-list 2) '(0 1))
(check-expect (for/build-list 10) '(0 1 2 3 4 5 6 7 8 9))

(define (for/build-list n)
  (for/list ([i n]) i))

;; Number -> [List-of Number]
;; creates the list '(0 ... n) for any natural number n

(check-expect (for/build-list-until-n 0) '())
(check-expect (for/build-list-until-n 1) '(1))
(check-expect (for/build-list-until-n 2) '(1 2))
(check-expect (for/build-list-until-n 10) '(1 2 3 4 5 6 7 8 9 10))

(define (for/build-list-until-n n)
  (for/list ([i n]) (add1 i)))

;; Number -> [List-of Number]
;; creates the list '(1 1/2 ... 1/n) for any natural number n

(check-expect (for/build-list-1/n 0) '())
(check-expect (for/build-list-1/n 1) '(1))
(check-expect (for/build-list-1/n 2) '(1 1/2))
(check-expect (for/build-list-1/n 10) '(1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10))

(define (for/build-list-1/n n)
  (for/list ([i n]) (/ 1 (add1 i))))

;; Number -> [List-of Number]
;; creates the list of the first n even numbers

(check-expect (for/build-list-of-n-even-nums 0) '())
(check-expect (for/build-list-of-n-even-nums 1) '(0))
(check-expect (for/build-list-of-n-even-nums 2) '(0 2))
(check-expect (for/build-list-of-n-even-nums 10) '(0 2 4 6 8 10 12 14 16 18))

(define (for/build-list-of-n-even-nums n)
  (for/list ([i n]) (* 2 i)))

;; a Row is one of:
;; - '()
;; - a list consisting of:
;;   - exactly one 1
;;   - more than zero 0s

;; an IdentityMatrix is a [List-of Rows]

;; Number -> IdentityMatrix
;; creates a IdentityMatrix of size (sqr n)

(check-expect (identity-matrix 0) '())
(check-expect (identity-matrix 1) '((1)))
(check-expect (identity-matrix 2) '((1 0) (0 1)))
(check-expect
 (identity-matrix 10)
 '((1 0 0 0 0 0 0 0 0 0)
   (0 1 0 0 0 0 0 0 0 0)
   (0 0 1 0 0 0 0 0 0 0)
   (0 0 0 1 0 0 0 0 0 0)
   (0 0 0 0 1 0 0 0 0 0)
   (0 0 0 0 0 1 0 0 0 0)
   (0 0 0 0 0 0 1 0 0 0)
   (0 0 0 0 0 0 0 1 0 0)
   (0 0 0 0 0 0 0 0 1 0)
   (0 0 0 0 0 0 0 0 0 1)))

(define (identity-matrix n)
  (for/list ([i n])
    (build-list n (lambda (x) (if (= x i) 1 0)))))

;; Number -> IdentityMatrix
;; like identity-matrix, creates a IdentityMatrix of size (sqr n)

(check-expect (identity-matrix.v2 0) '())
(check-expect (identity-matrix.v2 1) '((1)))
(check-expect (identity-matrix.v2 2) '((1 0) (0 1)))
(check-expect
 (identity-matrix.v2 10)
 '((1 0 0 0 0 0 0 0 0 0)
   (0 1 0 0 0 0 0 0 0 0)
   (0 0 1 0 0 0 0 0 0 0)
   (0 0 0 1 0 0 0 0 0 0)
   (0 0 0 0 1 0 0 0 0 0)
   (0 0 0 0 0 1 0 0 0 0)
   (0 0 0 0 0 0 1 0 0 0)
   (0 0 0 0 0 0 0 1 0 0)
   (0 0 0 0 0 0 0 0 1 0)
   (0 0 0 0 0 0 0 0 0 1)))

(define (identity-matrix.v2 n)
  (for/list ([i0 n]) ;; determines number of Rows
    (for/list ([i1 n]) ;; determines number of items on each Row
      (if (= i0 i1) 1 0)))) ;; determines whether item is 0 or 1

;; Number -> IdentityMatrix
;; like identity-matrix.v3, creates a IdentityMatrix of size (sqr n)

(check-expect (identity-matrix.v3 0) '())
(check-expect (identity-matrix.v3 1) '((1)))
(check-expect (identity-matrix.v3 2) '((1 0) (0 1)))
(check-expect
 (identity-matrix.v3 10)
 '((1 0 0 0 0 0 0 0 0 0)
   (0 1 0 0 0 0 0 0 0 0)
   (0 0 1 0 0 0 0 0 0 0)
   (0 0 0 1 0 0 0 0 0 0)
   (0 0 0 0 1 0 0 0 0 0)
   (0 0 0 0 0 1 0 0 0 0)
   (0 0 0 0 0 0 1 0 0 0)
   (0 0 0 0 0 0 0 1 0 0)
   (0 0 0 0 0 0 0 0 1 0)
   (0 0 0 0 0 0 0 0 0 1)))

(define (identity-matrix.v3 n)
  (build-list
   n
   (lambda (x)
     (for/list ([i n])
       (if (= i x) 1 0)))))

;; [Number] [Number -> Number] Number -> [List-of Number]
;; tabulates F between n and 0 (incl.) in a list

(check-expect (tabulate add1 0) '())
(check-expect (tabulate add1 1) '(1))
(check-expect (tabulate add1 3) '(3 2 1))
(check-expect (tabulate sqr 5) '(16 9 4 1 0))

(define (tabulate F n)
  (reverse
   (for/list ([i n])
     (F i))))

(test)
