#lang racket

;; a List-of-Names is one of:
;; - '()
;; - (cons String List-of-Names)
;; i.e. a list of invitees, by last name

(cons "last name 0"
      (cons "last name 1"
            (cons "last name 2"
                  (cons "last name 3"
                        (cons "last name 4" '())))))

(cons "1"
      (cons "2" '()))
;; this is an element of List-of-Names because "1"
;; "2" are Strings, which satisfy the second clause
;; of the data definition. compare with:
(cons 2 '())
;; where 2 is a Number, which doesn't make the list
;; a List-of-Names.
