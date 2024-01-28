#lang racket
(require test-engine/racket-tests)

;; NOTE: the functions that use min/max instead of </> are faster.
;; this is because the former performs fewer operations than the
;; latter, which recurses through its arguments per condition.

;; Non-Empty-List-of-Numbers -> Number
;; returns the smallest number on a-non-empty-list-of-numbers.

(check-expect (smallest-of '(0)) 0)
(check-expect (smallest-of '(0 1)) 0)
(check-expect (smallest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(check-expect (smallest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 1)

(define (smallest-of a-non-empty-list-of-numbers)
  (cond [(empty? (cdr a-non-empty-list-of-numbers)) (car a-non-empty-list-of-numbers)]
        [else (cond [(< (car a-non-empty-list-of-numbers)
                        (smallest-of (cdr a-non-empty-list-of-numbers)))
                     (car a-non-empty-list-of-numbers)]
                    [else (smallest-of (cdr a-non-empty-list-of-numbers))])]))

;; Non-Empty-List-of-Numbers -> Number
;; returns the biggest number on a-non-empty-list-of-numbers.

(check-expect (biggest-of '(0)) 0)
(check-expect (biggest-of '(0 1)) 1)
(check-expect (biggest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)
(check-expect (biggest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 25)

(define (biggest-of a-non-empty-list-of-numbers)
  (cond [(empty? (cdr a-non-empty-list-of-numbers)) (car a-non-empty-list-of-numbers)]
        [else (cond [(> (car a-non-empty-list-of-numbers)
                        (biggest-of (cdr a-non-empty-list-of-numbers)))
                     (car a-non-empty-list-of-numbers)]
                    [else (biggest-of (cdr a-non-empty-list-of-numbers))])]))

;; Comparison-Operator Non-Empty-List-of-Numbers -> Number
;; returns the Number on a-non-empty-list-of-numbers that holds true
;; when compared using a-comparator.

(check-expect (extract/number '(0) <) (smallest-of '(0)))
(check-expect (extract/number '(0 1) <) (smallest-of '(0 1)))
(check-expect (extract/number '(0) >) (biggest-of '(0)))
(check-expect (extract/number '(0 1) >) (biggest-of '(0 1)))
(check-expect (extract/number '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) <) 1)
(check-expect (extract/number '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) >) 25)
(check-expect (extract/number '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) <) 1)
(check-expect (extract/number '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) >) 25)

(define (extract/number a-non-empty-list-of-numbers a-comparator)
  (cond [(empty? (cdr a-non-empty-list-of-numbers)) (car a-non-empty-list-of-numbers)]
        [else (cond [(a-comparator (car a-non-empty-list-of-numbers)
                                   (extract/number (cdr a-non-empty-list-of-numbers) a-comparator))
                     (car a-non-empty-list-of-numbers)]
                    [else (extract/number (cdr a-non-empty-list-of-numbers) a-comparator)])]))

;; Non-Empty-List-of-Numbers -> Number
;; version 2 of smallest-of using extract/number.

(check-expect (smallest-of.v2 '(0)) (smallest-of '(0)))
(check-expect (smallest-of.v2 '(0 1)) (smallest-of '(0 1)))
(check-expect
 (smallest-of.v2 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 (smallest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
(check-expect
 (smallest-of.v2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
 (smallest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(define (smallest-of.v2 a-non-empty-list-of-numbers)
  (extract/number a-non-empty-list-of-numbers <))

;; Non-Empty-List-of-Numbers -> Number
;; version 2 of biggest-of using extract/number.

(check-expect (biggest-of.v2 '(0)) (biggest-of '(0)))
(check-expect (biggest-of.v2 '(0 1)) (biggest-of '(0 1)))
(check-expect
 (biggest-of.v2 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 (biggest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
(check-expect
 (biggest-of.v2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
 (biggest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(define (biggest-of.v2 a-non-empty-list-of-numbers)
  (extract/number a-non-empty-list-of-numbers >))

;; Non-Empty-List-of-Numbers -> Number
;; verson 3 of smallest-of using min instead of <.

(check-expect (smallest-of.v3 '(0)) (smallest-of '(0)))
(check-expect (smallest-of.v3 '(0 1)) (smallest-of '(0 1)))
(check-expect
 (smallest-of.v3 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 (smallest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
(check-expect
 (smallest-of.v3 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
 (smallest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(define (smallest-of.v3 a-non-empty-list-of-numbers)
  (cond [(empty? (cdr a-non-empty-list-of-numbers)) (car a-non-empty-list-of-numbers)]
        [else (min (car a-non-empty-list-of-numbers)
                   (smallest-of.v3 (cdr a-non-empty-list-of-numbers)))]))

;; Non-Empty-List-of-Numbers -> Number
;; verson 3 of smallest-of using min instead of <.

(check-expect (biggest-of.v3 '(0)) (biggest-of '(0)))
(check-expect (biggest-of.v3 '(0 1)) (biggest-of '(0 1)))
(check-expect
 (biggest-of.v3 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 (biggest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
(check-expect
 (biggest-of.v3 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
 (biggest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(define (biggest-of.v3 a-non-empty-list-of-numbers)
  (cond [(empty? (cdr a-non-empty-list-of-numbers)) (car a-non-empty-list-of-numbers)]
        [else (max (car a-non-empty-list-of-numbers)
                   (biggest-of.v3 (cdr a-non-empty-list-of-numbers)))]))

;; Comparison-Operator Non-Empty-List-of-Numbers -> Number
;; version 2 of extract/number.

(check-expect (extract/number.v2 '(0) min) (smallest-of '(0)))
(check-expect (extract/number.v2 '(0 1) min) (smallest-of '(0 1)))
(check-expect (extract/number.v2 '(0) max) (biggest-of '(0)))
(check-expect (extract/number.v2 '(0 1) max) (biggest-of '(0 1)))
(check-expect (extract/number.v2 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) min) 1)
(check-expect (extract/number.v2 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) max) 25)
(check-expect (extract/number.v2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) min) 1)
(check-expect (extract/number.v2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) max) 25)

(define (extract/number.v2 a-non-empty-list-of-numbers min/max)
  (cond [(empty? (cdr a-non-empty-list-of-numbers)) (car a-non-empty-list-of-numbers)]
        [else (min/max (car a-non-empty-list-of-numbers)
                       (extract/number.v2 (cdr a-non-empty-list-of-numbers) min/max))]))

;; Non-Empty-List-of-Numbers -> Number
;; version 4 of smallest-of using extract/number.v2.

(check-expect (smallest-of.v4 '(0)) (smallest-of '(0)))
(check-expect (smallest-of.v4 '(0 1)) (smallest-of '(0 1)))
(check-expect
 (smallest-of.v4 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 (smallest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
(check-expect
 (smallest-of.v4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
 (smallest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(define (smallest-of.v4 a-non-empty-list-of-numbers)
  (extract/number.v2 a-non-empty-list-of-numbers min))

;; Non-Empty-List-of-Numbers -> Number
;; version 4 of biggest-of using extract/number.v2.

(check-expect (biggest-of.v4 '(0)) (biggest-of '(0)))
(check-expect (biggest-of.v4 '(0 1)) (biggest-of '(0 1)))
(check-expect
 (biggest-of.v4 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 (biggest-of '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
(check-expect
 (biggest-of.v4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
 (biggest-of '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(define (biggest-of.v4 a-non-empty-list-of-numbers)
  (extract/number.v2 a-non-empty-list-of-numbers max))

(test)
