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

;; a List-of-list-of-1Strings (Ll1s)
;; for short is one of:
;; - '()
;; - (cons List-of-1Strings Ll1s)
;; examples:
(define ll1s0 '())
(define ll1s1 (list l1s1))
(define ll1s2 (list l1s1 l1s2))
(define ll1s3 (list l1s2 l1s3))
(define ll1s4 (list l1s1 l1s2 l1s3))
(define ll1s5 (list l1s2 l1s3 l1s4))
(define ll1s6 (list l1s2 l1s3 l1s4 l1s1))

;; an NE-Ll1s is one of:
;; - (cons List-of-1Strings '())
;; - (cons List-of-1Strings Ll1s)

;;; functions

;; ---------------------------------------------------------
;; prefixes
;; ---------------------------------------------------------

;; Ll1s -> Ll1s
;; returns a list of common prefixes in an Ll1s ll

(check-expect (prefixes ll1s0) '())
(check-expect (prefixes ll1s1) (list "a"))
(check-expect (prefixes ll1s2) (list "a"))
(check-expect (prefixes ll1s3) (list "a" "b"))
(check-expect (prefixes ll1s4) (list "a"))
(check-expect (prefixes ll1s5) '())
(check-expect (prefixes ll1s6) '())

(define (prefixes ll)
  (cond [(or (empty? ll) (empty? (first ll))) '()]
        [else (if (all-firsts=? ll)
                  (cons (first (first ll)) (prefixes (rest* ll)))
                  (prefixes '()))]))

;; NE-Ll1s -> NE-Ll1s
;; checks if the first 1Strings of each list
;; on an NE-Ll1s are equal

(check-expect (all-firsts=? ll1s1) #t)
(check-expect (all-firsts=? ll1s2) #t)
(check-expect (all-firsts=? ll1s3) #t)
(check-expect (all-firsts=? ll1s4) #t)
(check-expect (all-firsts=? ll1s5) #f)
(check-expect (all-firsts=? ll1s6) #f)

(define (all-firsts=? ll)
  (cond [(empty? (rest ll)) #t]
        [else (and (1string=? (first ll) (first (rest ll)))
                   (all-firsts=? (rest ll)))]))

;; List-of-1Strings List-of-1Strings -> Boolean
;; checks if the first 1Strings on both
;; Lists-of-1Strings are equal, if applicable

(check-expect (1string=? l1s0 l1s0) #f)
(check-expect (1string=? l1s1 l1s0) #f)
(check-expect (1string=? l1s1 l1s1) #t)
(check-expect (1string=? l1s1 l1s2) #t)
(check-expect (1string=? l1s1 l1s3) #t)
(check-expect (1string=? l1s1 l1s4) #f)

(define (1string=? l1 l2)
  (cond [(or (empty? l1) (empty? l2)) #f]
        [else (string=? (first l1) (first l2))]))

;; Ll1s -> Ll1s
;; removes the first 1String of each list in
;; an NE-Ll1s

(check-expect (rest* ll1s0) '())
(check-expect (rest* ll1s1) (list '()))
(check-expect (rest* ll1s2) (list '() (list "b")))
(check-expect (rest* ll1s3) (list (list "b") (list "b" "c")))
(check-expect (rest* ll1s4) (list '() (list "b") (list "b" "c")))
(check-expect (rest* ll1s5) (list (list "b") (list "b" "c") (list "b" "a")))
(check-expect (rest* ll1s6) (list (list "b") (list "b" "c") (list "b" "a") '()))

(define (rest* ll)
  (cond [(empty? ll) '()]
        [else (cons (remove-first (first ll)) (rest* (rest ll)))]))

;; List-of-1Strings -> List-of-1Strings
;; makes a new List-of-1Strings by removing
;; the first element in a List-of-1Strings l

(check-expect (remove-first l1s0) '())
(check-expect (remove-first l1s1) '())
(check-expect (remove-first l1s2) (list "b"))
(check-expect (remove-first l1s3) (list "b" "c"))
(check-expect (remove-first l1s4) (list "b" "a"))

(define (remove-first l)
  (if (empty? l) '() (rest l)))

;; ---------------------------------------------------------
;; suffixes
;; ---------------------------------------------------------

;; test cases:
(define l1s0-suffix '())
(define l1s1-suffix (list "c"))
(define l1s2-suffix (list "b" "c"))
(define l1s3-suffix (list "a" "b" "c"))
(define l1s4-suffix (list "c" "b" "a"))

(define ll1s0-suffix '())
(define ll1s1-suffix (list l1s1-suffix))
(define ll1s2-suffix (list l1s1-suffix l1s2-suffix))
(define ll1s3-suffix (list l1s2-suffix l1s3-suffix))
(define ll1s4-suffix (list l1s1-suffix l1s2-suffix l1s3-suffix))
(define ll1s5-suffix (list l1s2-suffix l1s3-suffix l1s4-suffix))
(define ll1s6-suffix (list l1s2-suffix l1s3-suffix l1s4-suffix l1s1-suffix))

;; Ll1s -> Ll1s
;; returns a list of common suffixes in an Ll1s ll

(check-expect (suffixes ll1s0-suffix) '())
(check-expect (suffixes ll1s1-suffix) (list "c"))
(check-expect (suffixes ll1s2-suffix) (list "c"))
(check-expect (suffixes ll1s3-suffix) (list "b" "c"))
(check-expect (suffixes ll1s4-suffix) (list "c"))
(check-expect (suffixes ll1s5-suffix) '())
(check-expect (suffixes ll1s6-suffix) '())

(define (suffixes ll)
  (reverse (prefixes (reverse-ll ll))))

;; Ll1s -> Ll1s
;; reverses all elements of each list on an Ll1s

(check-expect (reverse-ll ll1s0-suffix) '())
(check-expect (reverse-ll ll1s1-suffix)
              (list (list "c")))
(check-expect (reverse-ll ll1s2-suffix)
              (list (list "c") (list "c" "b")))
(check-expect (reverse-ll ll1s3-suffix)
              (list (list "c" "b") (list "c" "b" "a")))
(check-expect (reverse-ll ll1s4-suffix)
              (list (list "c") (list "c" "b") (list "c" "b" "a")))
(check-expect (reverse-ll ll1s5-suffix)
              (list (list "c" "b") (list "c" "b" "a") (list "a" "b" "c")))
(check-expect (reverse-ll ll1s6-suffix)
              (list (list "c" "b") (list "c" "b" "a") (list "a" "b" "c") (list "c")))

(define (reverse-ll ll)
  (cond [(empty? ll) '()]
        [else (cons (reverse (first ll)) (reverse-ll (rest ll)))]))

;;; application

(test)
