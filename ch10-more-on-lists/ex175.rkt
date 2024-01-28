#lang htdp/bsl
(require 2htdp/batch-io)
(require test-engine/racket-tests)

;;; data definitions

;; a List-of-strings is one of:
;; - '()
;; - (cons String List-of-strings)
;; i.e. a list of Strings, whether of words or 1Strings;
;; used to denote a line of text in a text file

;; an LLS is one of:
;; - '()
;; - (cons List-of-Strings List-of-List-of-strings)
;; i.e. a list of strings, one list per line and one
;; string per word; a.k.a. a list of list of strings
;; test cases:

(define line0 '())
(define line1 (cons "hello" (cons "world" '())))

(define lls0 '()) ;; simulates an empty text file
(define lls1 (cons line0 (cons line1 '())))
(define lls2 (cons line0 (cons line0 '()))) ;; simulates a text file with 2 empty lines

(define-struct wc [lines words 1strings])
;; a WC is a structure:
;;     (make-wc PositiveNumber PositiveNumber PositiveNumber)
;; i.e. a (make-wc lines words 1strings) is the collection
;; of lines, words, and 1strings counted in a given text file

;;; functions

;; String -> WC
;; using builtin Racket functions, simulates the Unix command
;; wc on a given text file tf

(check-expect (our-wc-short "ttt.txt") (make-wc 13 33 184))
(check-expect (our-wc-short "empty.txt") (make-wc 0 0 0))
(check-expect (our-wc-short "2-empty-lines.txt") (make-wc 2 0 2))

(define (our-wc-short tf)
  (make-wc (length (read-lines tf))
           (length (read-words tf))
           (if (empty? (read-1strings tf))
               (length (read-1strings tf))
               (add1 (length (read-1strings tf))))))

;; String -> String
;; using self-defined recursions, simulates the Unix command
;; wc on a given text file tf

(check-expect (our-wc-recursive "ttt.txt") (make-wc 13 33 184))
(check-expect (our-wc-recursive "empty.txt") (make-wc 0 0 0))
(check-expect (our-wc-recursive "2-empty-lines.txt") (make-wc 2 0 2))

(define (our-wc-recursive tf)
  (write-wc-info (read-words/line tf)))

;; LLS -> WC
;; returns a WC on a text file after the latter's been
;; converted to an LLS

(check-expect (write-wc-info lls0) (make-wc 0 0 0))
(check-expect (write-wc-info lls1) (make-wc 2 2 13))
(check-expect (write-wc-info lls2) (make-wc 2 0 2))

(define (write-wc-info lls)
  (make-wc (length lls)
           (count-words lls)
           (count-1strings lls)))

;; LLS -> Number
;; counts the number of words on an LLS

(check-expect (count-words lls0) 0)
(check-expect (count-words lls1) 2)

(define (count-words lls)
  (cond [(empty? lls) 0]
        [else (+ (words/line (first lls))
                 (count-words (rest lls)))]))

;; List-of-strings -> Number
;; counts the number of words on a line

(check-expect (words/line line0) 0)
(check-expect (words/line line1) 2)

(define (words/line ln)
  (cond [(empty? ln) 0]
        [else (add1 (words/line (rest ln)))]))

;; LLS -> Number
;; counts the number of 1Strings on an LLS

(check-expect (count-1strings lls0) 0)
(check-expect (count-1strings lls1) 13)
(check-expect (count-1strings lls2) 2)

(define (count-1strings lls)
  (cond [(empty? lls) 0]
        [else (+ (1string/line (first lls))
                 (count-1strings (rest lls)))]))

;; List-of-strings -> Number
;; counts the number of 1Strings on a line

(check-expect (1string/line line0) 1)
(check-expect (1string/line line1) 12)

(define (1string/line ln)
  (cond [(empty? ln) 1] ;; newline character
        [else (+ (if (not (empty? (rest ln))) 1 0) ;; spaces
                 (length (explode (first ln)))
                 (1string/line (rest ln)))]))

;;; application

(test)
