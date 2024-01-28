#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; data definitions

;; a Word is one of:
;; - '()
;; - (cons 1String Word)
;; i.e. a Word is a list of 1Strings (letters)
;; examples:
(define WORD-EXAMPLE-0 '())
(define WORD-EXAMPLE-1 (list "c" "a" "t"))
(define WORD-EXAMPLE-2 (list "a" "c" "t"))
(define WORD-EXAMPLE-3 (list "t" "a" "c"))
(define WORD-EXAMPLE-4 (list "r" "a" "t"))
(define WORD-EXAMPLE-5 (list "t" "a" "r"))
(define WORD-EXAMPLE-6 (list "a" "r" "t"))
(define WORD-EXAMPLE-7 (list "t" "r" "a"))

;; a List-of-Words is one of:
;; - '()
;; - (cons Word List-of-Words)
;; i.e. a list of Words.
;; examples:
(define LIST-OF-WORDS-EXAMPLE-0 '())
(define LIST-OF-WORDS-EXAMPLE-1
  (list WORD-EXAMPLE-1
        WORD-EXAMPLE-2))
(define LIST-OF-WORDS-EXAMPLE-2
  (list WORD-EXAMPLE-1
        WORD-EXAMPLE-2
        WORD-EXAMPLE-3))
(define LIST-OF-WORDS-EXAMPLE-3
  (list WORD-EXAMPLE-4
        WORD-EXAMPLE-5
        WORD-EXAMPLE-6))
(define LIST-OF-WORDS-EXAMPLE-4
  (list WORD-EXAMPLE-4
        WORD-EXAMPLE-5
        WORD-EXAMPLE-6
        WORD-EXAMPLE-7))

;;; functions

;;; application


(check-expect LIST-OF-WORDS-EXAMPLE-0 '())
(check-expect LIST-OF-WORDS-EXAMPLE-1 (list WORD-EXAMPLE-1 WORD-EXAMPLE-2))
(check-expect LIST-OF-WORDS-EXAMPLE-2 (list WORD-EXAMPLE-1 WORD-EXAMPLE-2 WORD-EXAMPLE-3))
(check-expect LIST-OF-WORDS-EXAMPLE-3 (list WORD-EXAMPLE-4 WORD-EXAMPLE-5 WORD-EXAMPLE-6))
(check-expect LIST-OF-WORDS-EXAMPLE-4 (list WORD-EXAMPLE-4 WORD-EXAMPLE-5 WORD-EXAMPLE-6 WORD-EXAMPLE-7))

(test)
