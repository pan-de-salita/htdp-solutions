#lang racket
(require 2htdp/batch-io)
(require test-engine/racket-tests)

;;; data definitions

;; an LN is one of:
;; - '()
;; - (cons List-of-Strings List-of-List-of-strings)
;; i.e. a list of strings, one list per line and one
;; string per word; a.k.a. a list of list of strings

(define line-ex0 '())
(define line-ex1
  (cons "this"
        (cons "is"
              (cons "line"
                    (cons "0" '())))))
(define line-ex2
  (cons "this"
        (cons "is"
              (cons "line"
                    (cons "1" '())))))

(define ln-ex0 '())
(define ln-ex1 (cons line-ex1 (cons line-ex0 '())))
(define ln-ex2 (cons line-ex1 (cons line-ex2 (cons line-ex0 '()))))
(define ln-ex3 (cons line-ex0 (cons line-ex1 (cons line-ex0 '()))))

(check-expect ln-ex0 '())
(check-expect ln-ex1
              (cons (cons "this"
                          (cons "is"
                                (cons "line"
                                      (cons "0" '()))))
                    (cons '() '())))
(check-expect ln-ex2
              (cons (cons "this"
                          (cons "is"
                                (cons "line"
                                      (cons "0" '()))))
                    (cons (cons "this"
                                (cons "is"
                                      (cons "line"
                                            (cons "1" '()))))
                          (cons '() '()))))
(check-expect ln-ex3
              (cons '()
                    (cons (cons "this"
                                (cons "is"
                                      (cons "line"
                                            (cons "0" '()))))
                          (cons '() '()))))

;;; functions

;; LN -> String
;; converts a list of list of strings into a string

(check-expect (collapse ln-ex0) "")
(check-expect (collapse ln-ex1) "this is line 0\n\n")
(check-expect (collapse ln-ex2) "this is line 0\nthis is line 1\n\n")
(check-expect (collapse ln-ex3) "\nthis is line 0\n\n")

(define (collapse ln)
  (cond [(empty? ln) ""]
        [else
         (string-append (ln->string (first ln))
                        (collapse (rest ln)))]))

;; List-of-strings -> String
;; converts a list of strings into a string then adds
;; a line character at its end

(check-expect (ln->string line-ex0) "\n")
(check-expect (ln->string line-ex1) "this is line 0\n")
(check-expect (ln->string line-ex2) "this is line 1\n")

(define (ln->string line)
  (cond [(empty? line) "\n"]
        [else
         (string-append (first line)
                        (if (empty? (rest line)) "" " ")
                        (ln->string (rest line)))]))

;;; application

(define collapse-challenge
  (write-file "ttt.dat"
              (collapse (read-words/line "ttt.txt"))))

(check-expect (read-words/line collapse-challenge)
              (read-words/line "ttt.txt"))

(test)
