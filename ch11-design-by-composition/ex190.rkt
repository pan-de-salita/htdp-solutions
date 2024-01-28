#lang racket
(require test-engine/racket-tests)

;;; data definitions

;; a List-of-1Strings is one of:
;; - '()
;; - (cons 1String List-of-1Strings)
;; examples:
(define l1s0 '())
(define l1s1 (list "a"))
(define l1s2 (list "a" "b"))
(define l1s3 (list "a" "b" "c"))
(define l1s4 (list "c" "b" "a"))

;; an NE-list-of-1Strings is one of:
;; - (cons 1String '())
;; - (cons 1String NE-list-of-1Strings)

;; a List-of-list-of-1Strings (Ll1s
;; for short) is one of:
;; - '()
;; - (cons List-of-1Strings Ll1s)

;;; functions

;; List-of-1Strings -> Ll1s
;; returns a list of all prefixes on a
;; List-of-1Strings

(check-expect (prefixes l1s0) '())
(check-expect (prefixes l1s1) (list (list "a")))
(check-expect (prefixes l1s2) (list (list "a" "b") (list "a")))
(check-expect (prefixes l1s3) (list (list "a" "b" "c") (list "a" "b") (list "a")))
(check-expect (prefixes l1s4) (list (list "c" "b" "a") (list "c" "b") (list "c")))

(define (prefixes l)
  (cond [(empty? l) '()]
        [else (cons l (prefixes (but-last l)))]))

;; NE-list-of-1Strtings -> NE-list-of-1Strings
;; removes the last element on an NE-list-of-1Strings

(check-expect (but-last l1s1) '())
(check-expect (but-last l1s2) (list "a"))
(check-expect (but-last l1s3) (list "a" "b"))
(check-expect (but-last l1s4) (list "c" "b"))

(define (but-last l)
  (reverse (rest (reverse l))))

;; List-of-1Strings -> Ll1s
;; returns a list of all suffixes on a
;; List-of-1Strings

(check-expect (suffixes l1s0) '())
(check-expect (suffixes l1s1) (list (list "a")))
(check-expect (suffixes l1s2) (list (list "a" "b") (list "b")))
(check-expect (suffixes l1s3) (list (list "a" "b" "c") (list "b" "c") (list "c")))
(check-expect (suffixes l1s4) (list (list "c" "b" "a") (list "b" "a") (list "a")))

(define (suffixes l)
  (cond [(empty? l) '()]
        [else (cons l (suffixes (rest l)))]))

;;; application

(test)
