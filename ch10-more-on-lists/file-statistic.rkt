#lang htdp/bsl
(require 2htdp/batch-io)
(require test-engine/racket-tests)

;; an LN is a List-of-list-of-strings, that is, one of:
;; - '()
;; - (cons List-of-strings List-of-list-of-strings)
;; i.e. a list of lines, each is a list of Strings

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(define lls2 (cons line1 (cons line0 '())))
(define lls3 (cons line1 (cons line1 '())))

(check-expect lls1
              (cons (cons "hello" (cons "world" '()))
                    (cons '() '())))

;; LN -> List-of-numbers
;; determines the number of words on each line

(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1) (cons 2 (cons 0 '())))
(check-expect (words-on-line lls2) (cons 0 (cons 2 '())))
(check-expect (words-on-line lls3) (cons 0 (cons 0 '())))

(define (words-on-line lls)
  (cond [(empty? lls) '()]
        [else
         (cons (words# (first lls))
               (words-on-line (rest lls)))]))

;; List-of-strings -> Number
;; determines the number of words on one line ln

(check-expect (words# line0) 2)
(check-expect (words# line1) 0)

(define (words# ln)
  (if (empty? ln)
      0
      (add1 (words# (rest ln)))))

;; LN -> List-of-numbers
;; counts the number of words on each line

(check-expect (words-on-line.v2 lls0) (words-on-line lls0))
(check-expect (words-on-line.v2 lls1) (words-on-line lls1))
(check-expect (words-on-line.v2 lls2) (words-on-line lls2))
(check-expect (words-on-line.v2 lls3) (words-on-line lls3))

(define (words-on-line.v2 lls)
  (if (empty? lls)
      '()
      (cons (length (first lls))
            (words-on-line (rest lls)))))

;; String -> List-of-numbers
;; counts the words on each line in the given file

(define (file-statistic file-name)
  (words-on-line (read-words/line file-name)))

(test)
