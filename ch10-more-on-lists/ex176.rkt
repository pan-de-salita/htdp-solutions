#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; data defitions

;; a Row is one of:
;; - '()
;; - (cons Number Row)
;; i.e. a row in a matrix

;; a Matrix is on of:
;; - (cons Row '())
;; - (cons Row Matrix)
;; constraint: all rows in matrix are of the same length

(define row0 '())
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))

(define matrix0 (cons row1 (cons row2 '())))

;;; functions

;; Matrix -> Matrix
;; transposes the given materix along the diagonal
;; NOTE: to transpose means to mirror the entries
;; along the diagonal, that is, the line from the
;; top-left to the bottom-right

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define xirtam0 (cons wor1 (cons wor2 '())))

(check-expect (transpose matrix0) xirtam0)

(define (transpose mat)
  (cond [(empty? (first mat)) '()]
        ;; (empty? (first mat)) is used because the input
        ;; transpose accepts is a Matrix, which by definition
        ;; cannot be an empty list. the smallest example of
        ;; a Matrix is (cons '() '()), which means there is
        ;; no longer any data manipulate and thus acts as
        ;; an appropriate base case for the function.
        [else (cons (first* mat) (transpose (rest* mat)))]))

;; Matrix -> Row
;; produces the first column of a matrix as a list of numbers

(check-expect (first* matrix0) wor1)

(define (first* mat)
  (cond [(empty? mat) '()]
        [else (cons (first (first mat)) (first* (rest mat)))]))

;; Matrix -> Row
;; removes the first column of a matrix

(check-expect (rest* matrix0)
              (cons (cons 12 '())
                    (cons (cons 22 '()) '())))

(define (rest* mat)
  (cond [(empty? mat) '()]
        [else (cons (rest (first mat)) (rest* (rest mat)))]))

;;; application

(define row3 (cons 1 (cons 2 (cons 3 '()))))
(define row4 (cons 4 (cons 5 (cons 6 '()))))
(define row5 (cons 7 (cons 8 (cons 9 '()))))
(define matrix2 (cons row3 (cons row4 (cons row5 '()))))

(define wor3 (cons 1 (cons 4 (cons 7 '()))))
(define wor4 (cons 2 (cons 5 (cons 8 '()))))
(define wor5 (cons 3 (cons 6 (cons 9 '()))))
(define xirtam2 (cons wor3 (cons wor4 (cons wor5 '()))))

(check-expect (transpose matrix2) xirtam2)

(test)

;; NOTE: transpose could not have been written with the design
;; recipes thus far discussed. this is because previous design
;; recipes treat lines of lines  of data by recursing through
;; their first elements one at a time. for example:
;;
;;     (define (work-through some-data)
;;       (cond [(empty? some-data) ...]
;;             [else (... (first (some-data))
;;                   (work-through (rest (some-data)))]))
;;
;; the above function consumes some-data, works on the first element
;; of the list of list via (first some-data), and then goes on to work
;; through the next element of the collection via (rest some-data).
;;
;; transpose behaves differently. applied to the function template
;; above, it would have worked on all the first elements of the list
;; of list of data via (first* some-data). it then will proceed to work
;; on all the first elements of the list of list of data via
;; (rest* (some data)) until the function reaches its base case.
