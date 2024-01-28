#lang racket
(require test-engine/racket-tests)
(require 2htdp/web-io)

;; List-of-Numbers List-of-Numbers -> ... nested list ...
;; creats an HTML table from two lists of numbers.

(check-expect
 (make-table '(1 2) '(3 4))
 (list 'table (list (list 'border "1"))
       (list 'tr (list 'td "1") (list 'td "2"))
       (list 'tr (list 'td "3") (list 'td "4"))))

(define (make-table row-1 row-2)
  `(table ((border "1"))
          (tr ,@(make-row row-1))
          (tr ,@(make-row row-2))))

;; List-of-Numbers -> ... nested list ...
;; creates a row for an HTML table from a List.

(check-expect (make-row '(1 2)) (list (list 'td "1") (list 'td "2")))

(define (make-row a-list-of-numbers)
  (cond [(empty? a-list-of-numbers) '()]
        [else (cons (make-cell (car a-list-of-numbers))
                    (make-row (cdr a-list-of-numbers)))]))

;; Number -> ... nested list ...
;; creates a cell for an HTML table from a Number.

(check-expect (make-cell 1) (list 'td "1"))

(define (make-cell a-number)
  `(td ,(number->string a-number)))

(test)
