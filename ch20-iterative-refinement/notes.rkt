#lang racket
(require test-engine/racket-tests)

#|

when dealing with complex forms of information and needing to
represent them with data, use iterative refinement. the key
is to find an accurate data representation of the real-world
information and fuctions that process them approriately.

data representaion and functions

ask:
- what are the essential pieces of information?
- what, given the predictions are not accurate enough, needs
  to be added?

|#

;;;; data representation of directory trees

;; v1
;; - files as atomic entities with a name
;; --- Strings
;; - directories as containers
;; --- Lists

;; a File.v1 is a String
(define-values
  (read!-10 part1 part2 part3 read!-19 hang draw)
  (values "read! (10)" "part1 (99)" "part2 (52)" "part3 (17)" "read! (19)" "hang (8)" "draw (2)"))

;; a Dir.v1 (short for Directory) is one of:
;; - '()
;; - (cons File.v1 Dir.v1)
;; - (cons Dir.v1 Dir.v1)
(define Code `(,hang ,draw))
(define Docs `(,read!-19))
(define Text `(,part1 ,part2 ,part3))
(define Libs `(,Code ,Docs))
(define TS `(,Text ,read!-10 ,Libs))
