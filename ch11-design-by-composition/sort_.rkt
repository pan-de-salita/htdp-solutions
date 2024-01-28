#lang racket
(require test-engine/racket-tests)

;;; data definitions

;; a List-of-numbers is one of:
;; - '()
;; - (cons Number List-of-numbers)
;; i.e. a list of numbers
;; examples:
(define alon0 '())
(define alon1 (list 1 2 3))
(define alon2 (list 3 2 1))
(define alon3 (list 12 20 -5))
(define alon4 (list 20 12 -5))

;; a NE-List-of-numbers is one of:
;; - (cons Number '())
;; - (cons Number NE-List-of-numbers)
;; i.e. a non-empty list of numbers
;; examples:
(define anelon0 alon1)
(define anelon1 alon2)
(define anelon2 alon3)
(define anelon3 alon4)

;;; functions

;; List-of-numbers -> List-of-numbers
;; rearranges a List-of-numbers alon in descending order

(check-expect (sort> alon0) '())
(check-satisfied (sort> alon1) sorted>?)
(check-satisfied (sort> alon2) sorted>?)
(check-satisfied (sort> alon3) sorted>?)
(check-satisfied (sort> alon4) sorted>?)

(define (sort> alon)
  (cond [(empty? alon) '()]
        [else (insert (car alon) (sort> (cdr alon)))]))

;; Number List-of-numbers -> List-of-numbers
;; inserts n into the sorted List-of-numbers alon

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

(define (insert n alon)
  (cond [(empty? alon) (cons n '())]
        [else (if (>= n (car alon))
                  (cons n alon)
                  (cons (car alon) (insert n (cdr alon))))]))

;; NEList-of-numbers -> Boolean
;; checks if the numbers in a List-of-numbers alon
;; are sorted in descending order

(check-expect (sorted>? anelon0) #f)
(check-expect (sorted>? anelon1) #t)
(check-expect (sorted>? anelon2) #f)
(check-expect (sorted>? anelon3) #t)

(define (sorted>? alon)
  (cond [(empty? (cdr alon)) #t]
        [else (and (>= (car alon) (cadr alon))
                   (sorted>? (cdr alon)))]))

;;; application

(test)
