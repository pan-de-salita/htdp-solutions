#lang racket

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
