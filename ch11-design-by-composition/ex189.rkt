#lang racket
(require test-engine/racket-tests)

;;; data definitions

;; a List-of-numbers is one of:
;; - '()
;; - (cons Number List-of-numbers)
;; i.e. a list of numbers
;; examples:
(define alon0 '())
(define alon1 (list 0))
(define alon2 (list 0 9))
(define alon3 (list 0 1 2 3 4 5 6 7 8 9))
(define alon4 (list 0 1 2 3 4 5 6 7 8 9 10))
(define alon5 (sort alon4 >))

;;; functions

;; Number List-of-numbers -> Boolean
;; checks if the given Number n occurs in the given
;; List-of-numbers alon

(check-expect (search 10 alon0) #f)
(check-expect (search 10 alon1) #f)
(check-expect (search 10 alon2) #f)
(check-expect (search 10 alon3) #f)
(check-expect (search 10 alon4) #t)
(check-expect (search 10 alon5) #t)

(define (search n alon)
  (cond [(empty? alon) #f]
        [else (or (= n (car alon))
                  (search n (cdr alon)))]))

;; Number List-of-numbers -> Boolean
;; checks if the given Number n occurs in the given
;; sorted list of numbers

(check-expect (search-sorted 10 alon0) #f)
(check-expect (search-sorted 10 alon1) #f)
(check-expect (search-sorted 10 alon2) #f)
(check-expect (search-sorted 10 alon3) #f)
(check-expect (search-sorted 10 alon4) #t)
(check-expect (search-sorted 10 alon5) #t)
(check-expect (search-sorted 11 alon4) #f)
(check-expect (search-sorted 11 alon5) #f)

(define (search-sorted n alon)
  (search n (sort alon >)))

;;; application

(test)
