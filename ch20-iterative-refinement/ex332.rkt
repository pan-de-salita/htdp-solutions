#lang racket
(require test-engine/racket-tests)

;; a File.v2 is a String
(define-values
  (read!-10.v2 part1.v2 part2.v2 part3.v2 read!-19.v2 hang.v2 draw.v2)
  (values "read! (10)" "part1 (99)" "part2 (52)" "part3 (17)" "read! (19)" "hang (8)" "draw (2)"))

;; problem with data representation of Dir.v1:
;; it obscured the nature of directories. we weren't
;; able to list all the names of the sub-directories
;; of some given directory.
;;
;; solution: introduce a structure type the combines
;; a name with a container.

(define-struct dir [name content])
;; a Dir.v2 is a structure:
;;   (make-dir String LOFD)

;; an LOFD (short for list of files and directories) is one of:
;; - '()
;; - (cons File.v2 LOFD)
;; - (cons Dir.v2 LOFD)

(define Code.v2
  (make-dir "Code"
            `(,hang.v2 ,draw.v2)))
(define Docs.v2
  (make-dir "Docs"
            `(,read!-19.v2)))
(define Text.v2
  (make-dir "Text"
            `(,part1.v2 ,part2.v2 ,part3.v2)))
(define Libs.v2
  (make-dir "Libs"
            `(,Code.v2 ,Docs.v2)))
(define TS.v2
  (make-dir "TS"
            `(,Text.v2 ,read!-10.v2 ,Libs.v2)))
