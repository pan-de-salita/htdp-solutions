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
;;   (make-dir.v3 String [List-of Dir.v3] [List-of File.v3])
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

;; Dir.v3 -> Number
;; returns the number of files in a dir.v3

(define (how-many.v3 dir.v3)
  (local (;; [List-of Dirs] -> Number
          (define (how-many-in-dirs l-dirs)
            (match l-dirs
              [(? null?) 0]
              [(cons fst rst)
               (+ (how-many.v3 fst)
                  (how-many-in-dirs rst))]))
          ;; [List-of Files] -> Number
          (define (how-many-in-files l-files)
            (match l-files
              [(? null?) 0]
              [(cons fst rst)
               (add1 (how-many-in-files rst))])))
    (+ (how-many-in-dirs (dir.v3-dirs dir.v3))
       (how-many-in-files (dir.v3-files dir.v3)))))

(check-expect (how-many.v3 Code.v3) 2)
(check-expect (how-many.v3 Docs.v3) 1)
(check-expect (how-many.v3 Text.v3) 3)
(check-expect (how-many.v3 Libs.v3) (+ (how-many.v3 Code.v3) (how-many.v3 Docs.v3)))
(check-expect (how-many.v3 TS.v3) (+ (how-many.v3 Text.v3) 1 (how-many.v3 Libs.v3)))

;; how-many.v3 produces the same results as how-many and how-many.v2
;; because each function ultimately extracts the same types of data
;; and performs the same forms of calculations regardless of the
;; increased complexity in each iteration of our data representation
;; models.

(test)
