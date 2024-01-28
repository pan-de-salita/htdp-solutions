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

(define Empty
  (make-dir "Empty"
            '()))
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

;; Dir.v2 -> Number
;; counts the amount of files in a dir.v2

(define (how-many.v2 dir.v2)
  (local (;; LOFD -> Number
          (define (how-many-in-content lofd)
            (cond
              [(null? lofd) 0]
              [(string? (car lofd))
               (add1 (how-many-in-content (cdr lofd)))]
              [else
               (+ (how-many.v2 (car lofd))
                  (how-many-in-content (cdr lofd)))])))
    (how-many-in-content (dir-content dir.v2))))

(check-expect (how-many.v2 Empty) 0)
(check-expect (how-many.v2 Code.v2) 2)
(check-expect (how-many.v2 Docs.v2) 1)
(check-expect (how-many.v2 Text.v2) 3)
(check-expect (how-many.v2 Libs.v2) (+ (how-many.v2 Code.v2) (how-many.v2 Docs.v2)))
(check-expect (how-many.v2 TS.v2) (+ (how-many.v2 Text.v2) 1 (how-many.v2 Libs.v2)))

;; Dir.v2 -> Number
;; like how-many.v2, counts the amount of files in a dir.v2

(define (how-many.v2-simplified dir.v2)
  (local (;; [File.v2 or Directory.v2] -> Number
          (define (how-many-in-content value)
            (if (string? value)
                1
                (how-many.v2-simplified value))))
    (foldr + 0 (map how-many-in-content (dir-content dir.v2)))))

(check-expect (how-many.v2-simplified Empty) (how-many.v2 Empty))
(check-expect (how-many.v2-simplified Code.v2) (how-many.v2 Code.v2))
(check-expect (how-many.v2-simplified Docs.v2) (how-many.v2 Docs.v2))
(check-expect (how-many.v2-simplified Text.v2) (how-many.v2 Text.v2))
(check-expect (how-many.v2-simplified Libs.v2) (how-many.v2 Libs.v2))
(check-expect (how-many.v2-simplified TS.v2) (how-many.v2 TS.v2))


(test)
