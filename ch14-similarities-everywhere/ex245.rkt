#lang racket
(require test-engine/racket-tests)

;; Function Function -> Boolean
;; determines whether two Number-taking functions (f1 f2)
;; produce the same results for 1.2, 3, and -5.775.

(check-expect (function=at-1.2-3-and-5.775? ret-0 ret-0-again) #t)
(check-expect (function=at-1.2-3-and-5.775? add1 sub1) #f)

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 -5.775) (f2 -5.775))))

;; Number -> Number
;; returns 0 no matter the number x given.

(check-expect (ret-0 0) 0)
(check-expect (ret-0 1) 0)
(check-expect (ret-0 (sub1 -10)) 0)

(define (ret-0 x) 0)

;; Number -> Number
;; returns 0 no matter the number y given.

(check-expect (ret-0-again 0) (ret-0 0))
(check-expect (ret-0-again 1) (ret-0 1))
(check-expect (ret-0-again (sub1 -10)) (ret-0 (sub1 -10)))

(define (ret-0-again y) 0)

;; it isn't possible to write function=? because it isn't possible
;; to account for the computation of every possible Number vis-a-vis
;; a function considering, among other reasons i haven't encountered,
;; the material limitatons of numerical representation on computers.
;; if, say, our two test functions are incapable of computing a certain
;; Number their host machine/s cannot represent due, for example, to
;; hardware restrictions, then there is no possibility of arriving
;; at an input/output, which despite being an unrepresentable Number,
;; is a number nonetheless.

(test)
