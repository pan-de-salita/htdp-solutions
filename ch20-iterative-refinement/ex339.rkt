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

;; Dir String -> Boolean
;; checks whether file exists within a dir

(define (find? dir file)
  (local (;; [List-of Dir] -> Boolean
          (define (found-in-dirs? dirs)
            (cond [(null? dirs) #f]
                  [else (or (find? (car dirs) file)
                        (found-in-dirs? (cdr dirs)))]))
          ;; [List-of File] -> Boolean
          (define (found-in-files? files)
            (ormap (lambda (f) (string=? (file-name f) file)) files)))
    (or (found-in-dirs? (dir-dirs dir))
        (found-in-files? (dir-files dir)))))

(check-expect (find? Code "read!") #f)
(check-expect (find? Docs "read!") #t)
(check-expect (find? Text "read!") #f)
(check-expect (find? Libs "read!") #t)
(check-expect (find? TS "read!") #t)

;; Dir String -> Boolean
;; like find?, checks whether file exists within a dir

(define (find?-simplified dir file)
  (or (ormap (lambda (f) (string=? (file-name f) file)) (dir-files dir))
      ;; S8A's version:
      ;; (not (false? (member file (map file-name (dir-files dir)))))
      (ormap (lambda (d) (find?-simplified d file)) (dir-dirs dir))))

(check-expect (find?-simplified Code "read!") (find? Code "read!"))
(check-expect (find?-simplified Docs "read!") (find? Docs "read!"))
(check-expect (find?-simplified Text "read!") (find? Text "read!"))
(check-expect (find?-simplified Libs "read!") (find? Libs "read!"))
(check-expect (find?-simplified TS "read!") (find? TS "read!"))

(test)
