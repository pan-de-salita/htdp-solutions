#lang htdp/bsl
(require 2htdp/batch-io)
(require test-engine/racket-tests)

;;; data definitions

;; an LLS is one of:
;; - '()
;; - (cons List-of-Strings List-of-List-of-strings)
;; i.e. a list of strings, one list per line and one
;; string per word; a.k.a. a list of list of strings
;; test cases:
(define line0 '())
(define line1 (cons "hello" (cons "world" '())))

(define lls0 '())
(define lls1 (cons line0 (cons line0 '())))
(define lls2 (cons line0 (cons line1 '())))
(define lls3 (cons line1 (cons line0 '())))
(define lls4 (cons line1 (cons line1 '())))

;;; functions

;; String -> String
;; converts all text in a text file n into numerics

(check-expect (text->numerics "ttt.txt")
              (write-file "numeric-ttt.txt"
                          (collapse->numerics (read-words/line "ttt.txt"))))
(check-expect (read-words/line (text->numerics "ttt.txt"))
              (read-words/line "numerical-ttt.txt"))

(define (text->numerics n)
  (write-file (string-append "numeric-" n)
              (collapse->numerics (read-words/line n))))

;; LLS -> String
;; converts an LLS into a string of numerics

(check-expect (collapse->numerics lls0) "")
(check-expect (collapse->numerics lls1) "\n\n")
(check-expect (collapse->numerics lls2) "\n104101108108111 119111114108100\n")
(check-expect (collapse->numerics lls3) "104101108108111 119111114108100\n\n")
(check-expect (collapse->numerics lls4) "104101108108111 119111114108100\n104101108108111 119111114108100\n")

(define (collapse->numerics lls)
  (cond [(empty? lls) ""]
        [else
         (string-append (ln->numerics (first lls))
                        (collapse->numerics (rest lls)))]))

;; List-of-strings -> List-of-1strings
;; converts a list of strings ln into a list-of-1string,
;; separating each string with a space, and then adding a
;; new-line character at the end

(check-expect (ln->numerics line0) "\n")
(check-expect (ln->numerics line1) "104101108108111 119111114108100\n")

(define (ln->numerics ln)
  (cond [(empty? ln) "\n"]
        [else
         (string-append (encode-letter (explode (first ln)))
                        (if (empty? (rest ln)) "" " ")
                        (ln->numerics (rest ln)))]))

;; List-of-1strings -> String
;; converts a List-of-1strings l1s into a numeric String

(check-expect (encode-letter line0) "")
(check-expect (encode-letter (explode (first line1))) "104101108108111")
(check-expect (encode-letter (explode (first (rest line1)))) "119111114108100")

(define (encode-letter l1s)
  (cond [(empty? l1s) ""]
        [else (string-append
               (cond [(>= (string->int (first l1s)) 100)
                      (code1 (first l1s))]
                     [(< (string->int (first l1s)) 10)
                      (string-append "00" (code1 (first l1s)))]
                     [(< (string->int (first l1s)) 100)
                      (string-append "0" (code1 (first l1s)))])
               (encode-letter (rest l1s)))]))

;; 1String -> String
;; converts the given 1String into a String

(check-expect (code1 "z") "122")

(define (code1 c)
  (number->string (string->int c)))

;;; application

(test)
