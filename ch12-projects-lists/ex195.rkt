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
(define dict-test2 (list "a" "as" "b" "c" "cat"))

;;; constants

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))

;;; functions

;; Letter Dictionary -> Number
;; counts how many times a word starts with the given
;; letter l in a dictionary d.

(check-expect (starts-with# "a" dict-test0) 0)
(check-expect (starts-with# "a" dict-test1) 2)
(check-expect (starts-with# "a" dict-test2) 2)

(define (starts-with# l d)
  (cond [(empty? d) 0]
        [else (if (string=? (substring (first d) 0 1) l)
                  (+ 1 (starts-with# l (rest d)))
                  (+ 0 (starts-with# l (rest d))))]))

;;; application

(test)
(starts-with# "a" DICTIONARY)
(starts-with# "b" DICTIONARY)
(starts-with# "c" DICTIONARY)
