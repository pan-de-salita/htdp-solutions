#lang racket
(require test-engine/racket-tests
         htdp/dir)

;; a File is a structure:
;;   (make-file String Number String)
(define-values
  (hang
   draw
   read!-19
   part1
   part2
   part3
   read!-10)
  (values (make-file "hang" 8 "")
          (make-file "draw" 2 "")
          (make-file "read!" 19 "")
          (make-file "part1" 99 "")
          (make-file "part2" 52 "")
          (make-file "part3" 17 "")
          (make-file "read!" 10 "")))

;; a Dir is a structure:
;;   (make-dir.v3 String [List-of Dir.v3] [List-of File.v3])
(define Code
  (make-dir "Code" '() `(,hang ,draw)))
(define Docs
  (make-dir "Docs" '() `(,read!-19)))
(define Text
  (make-dir "Text" '() `(,part1 ,part2 ,part3)))
(define Libs
  (make-dir "Libs" `(,Code ,Docs) '()))
(define TS
  (make-dir "TS" `(,Text ,Libs) `(,read!-10)))

(define htdp-ch1 (create-dir "/home/objekt0riented/Documents/htdp/ch1-arithmetic"))
(define htdp-ch2 (create-dir "/home/objekt0riented/Documents/htdp/ch2-functions-and-programs"))
(define htdp-ch3 (create-dir "/home/objekt0riented/Documents/htdp/ch3-how-to-design-programs"))
(define htdp-ch12 (create-dir "/home/objekt0riented/Documents/htdp/ch12-projects-lists"))

;; Dir -> Number
;; returns the number of files within a dir

(define (how-many dir)
  (foldr + (length (dir-files dir)) (map how-many (dir-dirs dir))))

(check-expect (how-many Code) 2)
(check-expect (how-many Docs) 1)
(check-expect (how-many Text) 3)
(check-expect (how-many Libs) (+ (how-many Code) (how-many Docs)))
(check-expect (how-many TS) (+ (how-many Text) 1 (how-many Libs)))
(check-expect (how-many htdp-ch1) 10)
(check-expect (how-many htdp-ch2) 34)
(check-expect (how-many htdp-ch3) 42)
(check-expect (how-many htdp-ch12) 67)

(test)
