#lang racket
(require test-engine/racket-tests)

;; a Son.L is one of:
;; - '()
;; - (cons Number Son.L)
;; Son is used when it applies to Son.L and Son.R

;; a Son.R is one of:
;; - '()
;; - (cons Number Son.R)
;; constraint: if is is a Son.R, no number occurs
;; twice in s

;; Son
(define es '())

;; Number Son -> Boolean
;; is x in s
(check-expect (in? 1 (cons 1 '())) #t)
(check-expect (in? 1 (cons 1 (cons 1 '()))) #t)
(check-expect (in? 1 (cons 2 '())) #f)
(check-expect (in? 1 '()) #f)

(define (in? x s)
  (cond [(empty? s) #f]
        [(cons? s)
         (or (= (first s) x)
             (in? x (rest s)))]))

;; Number Son.L -> Son.L
;; removes x from s
(define s1.L (cons 1 (cons 1 '())))
(check-expect (set-.L 1 s1.L) es)

(define (set-.L x s)
  (cond [(empty? s) s]
        [(cons? s)
         (if (= (first s) x)
             (set-.L x (rest s))
             (cons (first s) (set-.L x (rest s))))]))

;; Number Son.R -> Son.R
;; removes x from s
(define s1.R (cons 1 '()))
(check-expect (set-.R 1 s1.R) es)

(define (set-.R x s)
  (remove x s))

;; test cases for sets that don't end in an empty

(define set123-version1
  (cons 1 (cons 2 (cons 3 '()))))
(define set123-version2
  (cons 1 (cons 3 (cons 2 '()))))
(define set23-version1
  (cons 2 (cons 3 '())))
(define set23-version2
  (cons 3 (cons 2 '())))

(check-member-of (set-.L 1 set123-version1)
                 set23-version1
                 set23-version2)
(check-member-of (set-.R 1 set123-version1)
                 set23-version1
                 set23-version2)

;; Son -> Boolean
;; #true if 1 is not a member of s; #false otherwise
(define (not-member-1? s)
  (not (in? 1 s)))

(check-satisfied (set-.L 1 set123-version1) not-member-1?)
(check-satisfied (set-.L 1 set123-version2) not-member-1?)
(check-satisfied (set-.R 1 set123-version1) not-member-1?)
(check-satisfied (set-.R 1 set123-version2) not-member-1?)

(test)
