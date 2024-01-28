#lang racket
(require test-engine/racket-tests)

(define-struct file.v3 [name size content] #:transparent)
;; a File.v3 is a structure:
;;   (make-file String Number String)
(define-values
  (hang.v3
   draw.v3
   read!-19.v3
   part1.v3
   part2.v3
   part3.v3
   read!-10.v3)
  (values (make-file.v3 "hang" 8 "")
          (make-file.v3 "draw" 2 "")
          (make-file.v3 "read!" 19 "")
          (make-file.v3 "part1" 99 "")
          (make-file.v3 "part2" 52 "")
          (make-file.v3 "part3" 17 "")
          (make-file.v3 "read!" 10 "")))

(define-struct dir.v3 [name dirs files] #:transparent)
;; a Dir.v3 is a structure:
;;   (make-dir.v3 String
;;                [List-of Dir.v3]
;;                [List-of File.v3])
(define Code.v3
  (make-dir.v3 "Code" '() `(,hang.v3 ,draw.v3)))
(define Docs.v3
  (make-dir.v3 "Docs" '() `(,read!-19.v3)))
(define Text.v3
  (make-dir.v3 "Text" '() `(,part1.v3 ,part2.v3 part3.v3)))
(define Libs.v3
  (make-dir.v3 "Libs" `(,Code.v3 ,Docs.v3) '()))
(define TS.v3
  (make-dir.v3 "TS" `(,Text.v3 ,Libs.v3) `(,read!-10.v3)))
