#lang racket
(require 2htdp/batch-io)
(require test-engine/racket-tests)

;;; data definitions

;; an LLS is one of:
;; - '()
;; - (cons List-of-Strings List-of-List-of-strings)
;; i.e. a list of strings, one list per line and one
;; string per word; a.k.a. a list of list of strings

(define line0 '())
(define line1 (cons "a" (cons "cat" '())))
(define line2 (cons "an" (cons "angry" (cons "cat" '()))))
(define line3 (cons "the" (cons "sleepy" (cons "cat" '()))))

(define lls0 '())
(define lls1 (cons line1 (cons line2 (cons line3 (cons line0 '())))))
(define lls2 (cons line0 (cons line1 (cons line2 (cons line3 '())))))

;;; functions

;; String -> String
;; removes all articles from a text file

(check-expect (read-words/line (no-articles "ttt.txt"))
              (read-words/line "no-articles-ttt.dat"))

(define (no-articles n)
  (write-file (string-append "no-articles-" n)
              (collapse (remove-articles/lls (read-words/line n)))))

;; LLS -> LLS
;; removes all articles from an LLS lls

(check-expect (remove-articles/lls lls0) '())
(check-expect (remove-articles/lls lls1)
              (cons (cons "cat" '())
                    (cons (cons "angry"
                                (cons "cat" '()))
                          (cons (cons "sleepy"
                                      (cons "cat" '()))
                                (cons '() '())))))
(check-expect (remove-articles/lls lls2)
              (cons '()
                    (cons (cons "cat" '())
                          (cons (cons "angry"
                                      (cons "cat" '()))
                                (cons (cons "sleepy"
                                            (cons "cat" '())) '())))))

(define (remove-articles/lls lls)
  (cond [(empty? lls) '()]
        [else (cons (remove-articles/ln (first lls))
                    (remove-articles/lls (rest lls)))]))

;; List-of-strings -> List-of-strings
;; removes all articles from a list of strings ln

(check-expect (remove-articles/ln line0) '())
(check-expect (remove-articles/ln line1) (cons "cat" '()))
(check-expect (remove-articles/ln line2) (cons "angry" (cons "cat" '())))
(check-expect (remove-articles/ln line3) (cons "sleepy" (cons "cat" '())))

(define (remove-articles/ln ln)
  (cond [(empty? ln) '()]
        [else (if (article? (first ln))
                  (remove-articles/ln (rest ln))
                  (cons (first ln) (remove-articles/ln (rest ln))))]))

;; String -> String
;; checks if the given string s is one of:
;; - "a"
;; - "an"
;; - "the"

(check-expect (article? "a") #t)
(check-expect (article? "an") #t)
(check-expect (article? "the") #t)
(check-expect (article? "g") #f)
(check-expect (article? " ") #f)

(define (article? s)
  (or (string=? s "a")
      (string=? s "an")
      (string=? s "the")))

;; LN -> String
;; converts an LLS into a string

(check-expect (collapse lls0) "")
(check-expect (collapse lls1) "a cat\nan angry cat\nthe sleepy cat\n\n")
(check-expect (collapse lls2) "\na cat\nan angry cat\nthe sleepy cat\n")

(define (collapse lls)
  (cond [(empty? lls) ""]
        [else
         (string-append (ln->string (first lls))
                        (collapse (rest lls)))]))

;; List-of-strings -> String
;; converts a list of strings ln into a string, separating
;; each string with a space, and then adding a new-line
;; character at the end

(check-expect (ln->string line0) "\n")
(check-expect (ln->string line1) "a cat\n")
(check-expect (ln->string line2) "an angry cat\n")
(check-expect (ln->string line3) "the sleepy cat\n")

(define (ln->string ln)
  (cond [(empty? ln) "\n"]
        [else
         (string-append (first ln)
                        (if (empty? (rest ln)) "" " ")
                        (ln->string (rest ln)))]))

;;; application

(test)
