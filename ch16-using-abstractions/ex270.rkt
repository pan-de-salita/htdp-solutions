#lang racket
(require test-engine/racket-tests)

;; Number -> [List-of Number]
;; returns a list (list 0 ... (- n 1)) for any natural number n

(check-expect (build-list/add0 0) '()) ;; not a natural number
(check-expect (build-list/add0 1) (list 0))
(check-expect (build-list/add0 2) (list 0 1))
(check-expect (build-list/add0 3) (list 0 1 2))

(define (build-list/add0 n)
  (local (;; Number -> Number
          ;; adds 0 to a number x
          (define (add0 x) (+ x 0)))
    (build-list n add0)))

;; Number -> [List-of Number]
;; returns a list (list 1 ... n) for any natural number n

(check-expect (build-list/add1 0) '())
(check-expect (build-list/add1 1) (list 1))
(check-expect (build-list/add1 2) (list 1 2))
(check-expect (build-list/add1 3) (list 1 2 3))

(define (build-list/add1 n)
  (build-list n add1))

;; Number -> [List-of Number]
;; returns a list (list 1 1/2 ... 1/n) for any natural number n

(check-expect (build-list/add1/1-over-n 0) '())
(check-expect (build-list/add1/1-over-n 1) (list 1))
(check-expect (build-list/add1/1-over-n 2) (list 1 1/2))
(check-expect (build-list/add1/1-over-n 3) (list 1 1/2 1/3))

(define (build-list/add1/1-over-n n)
  (local (;; Number -> Number
          ;; returns a fraction with the structure 1/n
          (define (add1/1-over-n n) (/ 1 (add1 n))))
    (build-list n add1/1-over-n)))

;; Number -> [List-of Number]
;; returns a list a list of the first n even numbers

(check-expect (build-list/even 0) '())
(check-expect (build-list/even 1) '())
(check-expect (build-list/even 2) (list 2))
(check-expect (build-list/even 3) (list 2))
(check-expect (build-list/even 4) (list 2 4))

(define (build-list/even n)
  (filter even? (build-list n add1)))

;; identity matrices -----------------------------------------------------------

;; a Row is one of:
;; - a list of a single 1 and zero or more 0s
;; - or:
(define empty-row '())

;; an Identity-Matrix is a [List-of Rows] which, when stacked up,
;; has all 1s running diagonally starting from the top left to
;; the bottom right; examples:
(define identity-matrix-0 empty-row)
(define identity-matrix-1 '((1)))
(define identity-matrix-2
  '((1 0)
    (0 1)))
(define identity-matrix-3
  '((1 0 0)
    (0 1 0)
    (0 0 1)))
(define identity-matrix-4
  '((1 0 0 0)
    (0 1 0 0)
    (0 0 1 0)
    (0 0 0 1)))

;; Number -> Identity-Matrix
;; returns an identity matrix of size (* n n)
;; NOTE: original logic by S8A:
;;   https://github.com/S8A/htdp-exercises/blob/master/ex270.rkt

(check-expect (identity-matrix 0) identity-matrix-0)
(check-expect (identity-matrix 1) identity-matrix-1)
(check-expect (identity-matrix 2) identity-matrix-2)
(check-expect (identity-matrix 3) identity-matrix-3)
(check-expect (identity-matrix 4) identity-matrix-4)

(define (identity-matrix n)
  (cond [(zero? n) empty-row]
        [else (local (;; 1. build first row of identity matrix:
                      (define first-row (build-list n (lambda (x) (if (zero? x) 1 0))))
                      ;; 2. build identity matrix of size (* (sub1 n) (sub1 n)):
                      (define identity-matrix/sub1-n (identity-matrix (sub1 n)))
                      ;; 3. prepend 0 to each row of identity-matrix/sub1-n:
                      (define identity-matrix/cdr (map (lambda (x) (cons 0 x)) identity-matrix/sub1-n)))
                ;; cons first row of identify matrix onto cdr of identity matrix:
                (cons first-row identity-matrix/cdr))]))

;; Number -> Identity-Matrix
;; identity-matrix.v2 works just like identity-matrix

(check-expect (identity-matrix.v2 0) (identity-matrix 0))
(check-expect (identity-matrix.v2 1) (identity-matrix 1))
(check-expect (identity-matrix.v2 2) (identity-matrix 2))
(check-expect (identity-matrix.v2 3) (identity-matrix 3))
(check-expect (identity-matrix.v2 4) (identity-matrix 4))

(define (identity-matrix.v2 n)
  (local (;; Number Number -> Identity-Matrix
          ;; builds an identity matrix of size (* x x) Row by Row
          (define (build-identity-matrix x [counter 0])
            (local (;; Number -> Row
                    ;; builds a Row of length y
                    (define (build-row y position-of-one)
                      (build-list y (lambda (z) (if (= z position-of-one) 1 0)))))
              (cond [(= x counter) '()]
                    [else (cons (build-row x counter)
                                (build-identity-matrix x (add1 counter)))]))))
    (build-identity-matrix n)))

;; Number -> Identity-Matrix
;; identity-matrix.v3 works just like identity-matrix
;; and identity-matrix.v2
;; NOTE: original logic by Y.E.
;;   https://gitlab.com/cs-study/htdp/-/blob/main/03-Abstraction/16-Using-Abstractions/Exercise-270.rkt

(check-expect (identity-matrix.v3 0) (identity-matrix.v2 0))
(check-expect (identity-matrix.v3 1) (identity-matrix.v2 1))
(check-expect (identity-matrix.v3 2) (identity-matrix.v2 2))
(check-expect (identity-matrix.v3 3) (identity-matrix.v2 3))
(check-expect (identity-matrix.v3 4) (identity-matrix.v2 4))

(define (identity-matrix.v3 row-length)
  (local (;; Number -> Row
          ;; builds each row of an identity matrix
          (define (build-rows row-number)
            (local (;; Number -> Number
                    ;; inserts the appropriate digit into a Row:
                    ;; - 1 if row-number equals digit-index
                    ;; - 0 otherwise
                    (define (insert-0-or-1 digit-index)
                      (if (= row-number digit-index) 1 0)))
              (build-list row-length insert-0-or-1))))
    (build-list row-length build-rows)))

;; Number -> Identity-Matrix
;; identity-matrix.v4 works just like indentity-matrix

(check-expect (identity-matrix.v4 0) (identity-matrix.v3 0))
(check-expect (identity-matrix.v4 1) (identity-matrix.v3 1))
(check-expect (identity-matrix.v4 2) (identity-matrix.v3 2))
(check-expect (identity-matrix.v4 3) (identity-matrix.v3 3))
(check-expect (identity-matrix.v4 4) (identity-matrix.v3 4))

(define (identity-matrix.v4 n)
  (cond [(zero? n) '()]
        [else (local (;; 1. create a Row of only 0s:
                      (define row-of-0s (make-list (sub1 n) 0))
                      ;; 2. insert 1 into all possible positions within row-of-0s:
                      ;; Row -> Identity-Matrix
                      (define (insert-1-everywhere a-row-of-0s)
                        (cond [(empty? a-row-of-0s) (list (list 1))]
                              [else (local ((define inserted-1-everywhere/cdr
                                              (insert-1-everywhere (cdr a-row-of-0s)))
                                            ;; Row -> Row
                                            ;; prepends 0 to a Row
                                            (define (prepend-0-to a-row) (cons 0 a-row)))
                                      (cons (cons 1 a-row-of-0s)
                                            (map prepend-0-to inserted-1-everywhere/cdr)))])))
                (insert-1-everywhere row-of-0s))]))

;; Number -> [List-of Row]
;; identity-matrix.v5 works just like identity-matrix

(check-expect (identity-matrix.v5 0) (identity-matrix.v3 0))
(check-expect (identity-matrix.v5 1) (identity-matrix.v3 1))
(check-expect (identity-matrix.v5 2) (identity-matrix.v3 2))
(check-expect (identity-matrix.v5 3) (identity-matrix.v3 3))
(check-expect (identity-matrix.v5 4) (identity-matrix.v3 4))

(define (identity-matrix.v5 a-number)
  (local (;; Number -> [List-of Number]
          ;; builds each Row of the identity matrix
          (define (per-row x [y (sub1 a-number)])
            (cond [(< y 0) '()]
                  [else (cons (if (= x y) 1 0)
                              (per-row x (sub1 y)))])))
    (reverse (build-list a-number per-row))))

;; Number -> [List-of Row]
;; identity-matrix.v6 works just like identity-matrix

(check-expect (identity-matrix.v6 0) (identity-matrix.v3 0))
(check-expect (identity-matrix.v6 1) (identity-matrix.v3 1))
(check-expect (identity-matrix.v6 2) (identity-matrix.v3 2))
(check-expect (identity-matrix.v6 3) (identity-matrix.v3 3))
(check-expect (identity-matrix.v6 4) (identity-matrix.v3 4))

(define (identity-matrix.v6 a-number)
  (local (;; Number [List-of Row] -> [List-of Row]
          ;; makes each Row and
          ;; conses it onto the existing identity matrix
          (define (make-row index matrix)
            (local (;; Number -> Row
                    ;; inserts either 1 or 0 depending on
                    ;; given index
                    (define (zero-or-one x) (if (= x index) 1 0)))
              (cons (build-list a-number zero-or-one) matrix))))
    (foldr make-row '() (build-list a-number append))))

;; -----------------------------------------------------------------------------

;; Number [Number -> Number] -> [List-of Number]
;; applies a numerical function applied to n and 0 (incl.) and tabulates
;; the results in a list

(check-expect (tabulate 0 add1) '(1))
(check-expect (tabulate 1 add1) '(2 1))
(check-expect (tabulate 5 add1) '(6 5 4 3 2 1))
(check-within (tabulate 2 sqrt) (list (sqrt 2) 1 0) 0.001)

(define (tabulate n a-numerical-function)
  (reverse (build-list (add1 n) a-numerical-function)))

(test)
