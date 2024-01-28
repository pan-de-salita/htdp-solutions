#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; data definitions

;; a Letter is one of the following 1Strings:
;; - "a"
;; - ...
;; - "z"
;; i.e. any character of the English alphabet.

;; a Dictionary is a List-of-Strings, that is one of:
;; - '()
;; - (cons String Dictionary)
;; i.e. a list of words.
;; examples:
(define dict-test0 '())
(define dict-test1 (list "a" "as"))
(define dict-test2 (list "a" "as" "b" "c" "cat" "code"))
(define dict-test3
  (list "bowl" "racket" "notebook"
        "pen" "pan" "keyboard" "water"
        "ball" "watch" "will"))

(define-struct lc [letter count] #:transparent)
;; an LC (short for Letter-Count) is a structure:
;;     (make-lc Letter PositiveNumber)
;; i.e. a piece of data describing how many times a
;; letter figures as the first letter in a Dictionary.
;; examples:
(define lc-test0 (make-lc "a" 2))
(define lc-test1 (make-lc "b" 1))
(define lc-test2 (make-lc "c" 3))

;; a List-of-LCs is one of:
;; - '()
;; - (cons LC List-of-LCs)
;; i.e. a list of Letter-Counts.
;; examples:
(define llc-test0 '())
(define llc-test1 (list lc-test0))
(define llc-test2 (list lc-test0 lc-test1 lc-test2))
(define llc-test3
  (list (make-lc "w" 3)
        (make-lc "b" 2)
        (make-lc "k" 1)
        (make-lc "p" 2)
        (make-lc "n" 1)
        (make-lc "r" 1)))

;;; constants

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))

;;; functions

;; Dictionary -> LC
;; returns the LC for the letter that occurs most often as
;; the first letter in the given dictionary.

(check-expect (most-frequent dict-test0) '())
(check-expect (most-frequent dict-test1) (make-lc "a" 2))
(check-expect (most-frequent dict-test2) (make-lc "c" 3))
(check-expect (most-frequent dict-test3) (make-lc "w" 3))

(define (most-frequent dict)
  (if (empty? dict)
      '()
      (first (sort>-llc (count-by-letter dict)))))

;; ;; List-of-LCs -> list-of-LCs
;; ;; sorts a List-of-LCs according to its lc-counts in
;; ;; descending order

(check-expect (sort>-llc llc-test0) '())
(check-expect (sort>-llc llc-test1) llc-test1)
(check-expect (sort>-llc llc-test2) (list lc-test2 lc-test0 lc-test1))
(check-expect
 (sort>-llc llc-test3)
 (list (make-lc "w" 3)
       (make-lc "b" 2)
       (make-lc "p" 2)
       (make-lc "k" 1)
       (make-lc "n" 1)
       (make-lc "r" 1)))

(define (sort>-llc llc)
  (cond [(empty? llc) '()]
        [else (insert-sorted (first llc) (sort>-llc (rest llc)))]))

;; LC List-of-LCs -> List-of-LCs
;; inserts the given LC into a sorted List-of-LCs

(check-expect (insert-sorted (make-lc "d" 10) llc-test0) (list (make-lc "d" 10)))
(check-expect (insert-sorted (make-lc "d" 10) llc-test1) (cons (make-lc "d" 10) llc-test1))
(check-expect (insert-sorted (make-lc "d" 10) llc-test2) (cons (make-lc "d" 10) llc-test2))
(check-expect
 (insert-sorted (make-lc "k" 0) llc-test3)
 (list (make-lc "w" 3)
       (make-lc "b" 2)
       (make-lc "k" 1)
       (make-lc "p" 2)
       (make-lc "n" 1)
       (make-lc "r" 1)
       (make-lc "k" 0)))

(define (insert-sorted lc llc)
  (cond [(empty? llc) (cons lc llc)]
        [else (if (>= (lc-count lc) (lc-count (first llc)))
                  (cons lc llc)
                  (cons (first llc) (insert-sorted lc (rest llc))))]))

;; Dictionary -> List-of-LCs
;; counts how often each letter figures  as the first letter
;; of a word in a given Dictionary d.

(check-expect (count-by-letter dict-test0) llc-test0)
(check-expect (count-by-letter dict-test1) llc-test1)
(check-expect (count-by-letter dict-test2) (reverse llc-test2))
(check-expect (count-by-letter dict-test3) llc-test3)

(define (count-by-letter dict)
  (cond [(empty? dict) '()]
        [else (llc-maker (substring (first dict) 0 1)
                         (count-by-letter (rest dict)))]))

;; Letter List-of-LCs -> List-of-LCs
;; modifies a given List-of-LCs llc depending on:
;; - if the Letter figures as an LC, increment its count by 1
;; - if a given Letter doesn't figure as an LC, add it

(check-expect (llc-maker "a" llc-test0) (list (make-lc "a" 1)))
(check-expect (llc-maker "a" llc-test1) (list (make-lc "a" 3)))
(check-expect (llc-maker "a" llc-test2) (list (make-lc "a" 3) (make-lc "b" 1) (make-lc "c" 3)))
(check-expect (llc-maker "d" llc-test2)
              (list (make-lc "a" 2)
                    (make-lc "b" 1)
                    (make-lc "c" 3)
                    (make-lc "d" 1)))
(check-expect (llc-maker "f" llc-test3)
              (list (make-lc "w" 3)
                    (make-lc "b" 2)
                    (make-lc "k" 1)
                    (make-lc "p" 2)
                    (make-lc "n" 1)
                    (make-lc "r" 1)
                    (make-lc "f" 1)))

(define (llc-maker letter llc)
  (cond [(empty? llc) (cons (make-lc letter 1) '())]
        [else (if (not (string=? letter (lc-letter (first llc))))
                  (cons (first llc) (llc-maker letter (rest llc)))
                  (cons (make-lc letter (add1 (lc-count (first llc)))) (rest llc)))]))

;;; application

(test)
(most-frequent DICTIONARY)
