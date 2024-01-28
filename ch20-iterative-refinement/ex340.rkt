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

;; Dir -> [List-of String]
;; lists the names of all Files and Dirs in dir

(define (ls dir)
  (local (;; [List-of Dirs] -> [List-of String]
          (define (ls-dirs dirs)
            (cond [(null? dirs) '()]
                  [else (append
                         (cons (string-append (dir-name (car dirs)) "/")
                               (map (lambda (f)
                                      (string-append (dir-name (car dirs)) "/" f))
                                    (ls (car dirs))))
                         (ls-dirs (cdr dirs)))]))
          ;; [List-of File] -> [List-of String]
          (define (ls-files files)
            (cond [(null? files) '()]
                  [else (cons (file-name (car files))
                              (ls-files (cdr files)))])))
    (sort
     (append (ls-dirs (dir-dirs dir))
             (ls-files (dir-files dir)))
     string<?)))

(check-expect (ls Code) '("draw" "hang"))
(check-expect (ls Docs) '("read!"))
(check-expect (ls Text) '("part1" "part2" "part3"))
(check-expect (ls Libs)
              '("Code/"
                "Code/draw"
                "Code/hang"
                "Docs/"
                "Docs/read!"))
(check-expect (ls TS)
              '("Libs/"
                "Libs/Code/"
                "Libs/Code/draw"
                "Libs/Code/hang"
                "Libs/Docs/"
                "Libs/Docs/read!"
                "Text/"
                "Text/part1"
                "Text/part2"
                "Text/part3"
                "read!"))

;; Dir -> [List-of String]
;; like ls, lists the names of all Files and Dirs in dir

(define (ls-simplified dir)
  (sort
   (append (foldr
            (lambda (d ls-d)
              (append
               (cons (string-append (dir-name d) "/")
                     (map (lambda (f)
                            (string-append (dir-name d) "/" f))
                          (ls-simplified d)))
                      ls-d))
            '()
            (dir-dirs dir))
           (map file-name (dir-files dir)))
   string<?))

(check-expect (ls-simplified Code) (ls Code))
(check-expect (ls-simplified Docs) (ls Docs))
(check-expect (ls-simplified Text) (ls Text))
(check-expect (ls-simplified Libs) (ls Libs))
(check-expect (ls-simplified TS) (ls TS))

(test)
