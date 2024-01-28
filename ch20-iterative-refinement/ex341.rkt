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
;; computes the total size of all Files in a Dir,
;; nested Dirs included

(define (du dir)
  (add1 ;; the size of the parent/base directory
   (foldr +
          (foldr + 0 (map file-size (dir-files dir)))
          (map du (dir-dirs dir)))
   ;; S8A's solution:
   ;; (local (define dir-size 1)
   ;;   (foldr + dir-size (append (map (file-size (dir-files dir))) (map du (dir-dirs dir)))))
   ))

(check-expect (du (make-dir "Empty Dir" '() '())) 1)
(check-expect (du Code) 11)
(check-expect (du Docs) 20)
(check-expect (du Text) 169)
(check-expect (du Libs) (+ (du Code) (du Docs) 1))
(check-expect (du TS) (+ (du Text) (du Libs) 10 1))

(test)
