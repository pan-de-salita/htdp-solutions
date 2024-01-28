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
(define TS-TS
  (make-dir "TS-TS" `(,TS ,TS) `(,read!-10)))
(define TS-TSTS-TS-TSTS
  (make-dir "TS-TSTS-TS-TSTS" `(,TS ,TS-TS ,TS ,TS-TS) `(,read!-10 ,draw ,read!-10)))

(define htdp-ch1 (create-dir "/home/objekt0riented/Documents/htdp/ch1-arithmetic"))
(define htdp-ch2 (create-dir "/home/objekt0riented/Documents/htdp/ch2-functions-and-programs"))
(define htdp-ch3 (create-dir "/home/objekt0riented/Documents/htdp/ch3-how-to-design-programs"))
(define htdp-ch12 (create-dir "/home/objekt0riented/Documents/htdp/ch12-projects-lists"))

;; a Path is a [List-of String]
;; i.e. directions into a directory tree

;; Dir -> [List-of Path]
;; returns lists of paths to all Files in directory
;; sans its parent dir-name

(define (ls-R directory)
  (local (;; [List-of Dirs] -> [List-of Path]
          (define (ls-R-dirs dirs)
            (foldr append
                   '()
                   (map (lambda (dir)
                          (map (lambda (path)
                                 (cons (dir-name dir) path))
                               (ls-R dir)))
                        dirs)))
          ;; [List-of Files] -> [List-of Path]
          (define (ls-R-files files)
            (map (lambda (file) (list (file-name file))) files)))
    (append (ls-R-dirs (dir-dirs directory))
            (ls-R-files (dir-files directory)))))

(check-expect (ls-R Code)
              '(("hang")
                ("draw")))
(check-expect (ls-R Docs)
              '(("read!")))
(check-expect (ls-R Text)
              '(("part1")
                ("part2")
                ("part3")))
(check-expect (ls-R Libs)
              '(("Code" "hang")
                ("Code" "draw")
                ("Docs" "read!")))
(check-expect (ls-R TS)
              '(("Text" "part1")
                ("Text" "part2")
                ("Text" "part3")
                ("Libs" "Code" "hang")
                ("Libs" "Code" "draw")
                ("Libs" "Docs" "read!")
                ("read!")))

;; Dir -> [List-of Path]
;; returns lists of paths to all Files in directory
;; with its parent dir-name

(define (ls-R/with-parent directory)
  (local (;; [List-of Dir] -> [List-of Path]
          (define (ls-R-dirs/with-parent dirs)
            (foldr append '() (map ls-R/with-parent dirs)))
          ;; [List-of File] -> [List-of Path]
          (define (ls-R-files/with-parent files)
            (map (lambda (file) (list (file-name file))) files))
          ;; [List-of Path] -> [List-of Path]
          (define (append-parent-dir paths)
            (map (lambda (path) (cons (dir-name directory) path)) paths)))
    (append-parent-dir
     (append (ls-R-dirs/with-parent (dir-dirs directory))
             (ls-R-files/with-parent (dir-files directory))))))

(check-expect (ls-R/with-parent Code)
              '(("Code" "hang")
                ("Code" "draw")))
(check-expect (ls-R/with-parent Docs)
              '(("Docs" "read!")))
(check-expect (ls-R/with-parent Text)
              '(("Text" "part1")
                ("Text" "part2")
                ("Text" "part3")))
(check-expect (ls-R/with-parent Libs)
              '(("Libs" "Code" "hang")
                ("Libs" "Code" "draw")
                ("Libs" "Docs" "read!")))
(check-expect (ls-R/with-parent TS)
              '(("TS" "Text" "part1")
                ("TS" "Text" "part2")
                ("TS" "Text" "part3")
                ("TS" "Libs" "Code" "hang")
                ("TS" "Libs" "Code" "draw")
                ("TS" "Libs" "Docs" "read!")
                ("TS" "read!")))

;; Dir String -> [List-of Path]
;; returns the list of all paths in dir that lead to x
;; (x here can either be a file name or directory name)

(define (find-all.v3 dir x)
  (filter (lambda (path) (string=? x (last path))) (ls-R/with-parent dir)))

(check-expect (find-all.v3 Code "read!") '())
(check-expect (find-all.v3 Docs "read!") '(("Docs" "read!")))
(check-expect (find-all.v3 Text "read!") '())
(check-expect (find-all.v3 Libs "read!") '(("Libs" "Docs" "read!")))
(check-expect (find-all.v3 TS "read!")
              '(("TS" "Libs" "Docs" "read!")
                ("TS" "read!")))
(check-expect (find-all.v3 TS-TS "read!")
              '(("TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TS" "TS" "read!")
                ("TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TS" "TS" "read!")
                ("TS-TS" "read!")))
(check-expect (find-all.v3 TS-TSTS-TS-TSTS "read!")
              '(("TS-TSTS-TS-TSTS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "read!")
                ("TS-TSTS-TS-TSTS" "read!")
                ("TS-TSTS-TS-TSTS" "read!")))

(test)
