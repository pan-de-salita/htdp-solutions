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

;; Dir String -> Boolean
;; checks whether a File figures in a Dir

(define (found? dir file)
  (local (;; [List-of File] -> Boolean
          (define (found-in-files? files)
            (ormap (lambda (f) (string=? f file))
                   (map file-name files)))
          ;; [List-of Dir] -> Boolean
          (define (found-in-dirs? dirs)
            (ormap (curryr found? file) dirs)))
    (or (found-in-files? (dir-files dir))
        (found-in-dirs? (dir-dirs dir)))))

(check-expect (found? Code "read!") #f)
(check-expect (found? Docs "read!") #t)
(check-expect (found? Text "read!") #f)
(check-expect (found? Libs "read!") #t)
(check-expect (found? TS "read!") #t)

;; Dir String -> Path
;; returns a Path to a File if it figures in a Dir,
;; else returns #f

(define (find dir file)
  (if (not (found? dir file))
      #f
      (local (;; [List-of File] [List-of Dirs] -> Path
              (define (build-path files dirs)
                (local (;; [List-of File] -> Boolean
                        (define (file-in-files? files)
                          (not (false? (member file (map file-name files)))))
                        ;; [List-of File] -> String
                        (define (build-path-from-files files)
                          (list file))
                        ;; [List-of Dir] -> Path
                        (define (build-path-from-dirs dirs)
                          (local ((define dir-holding-file
                                    (car (filter (lambda (d) (found? d file)) dirs))))
                            (find dir-holding-file file))))
                  (if (file-in-files? files)
                      (build-path-from-files files)
                      (build-path-from-dirs dirs)))))
        (append (list (dir-name dir))
                (build-path (dir-files dir) (dir-dirs dir))))))

(check-expect (find Code "read!") #f)
(check-expect (find Docs "read!") '("Docs" "read!"))
(check-expect (find Text "read!") #f)
(check-expect (find Libs "read!") '("Libs" "Docs" "read!"))
(check-expect (find TS "read!") '("TS" "read!"))

;; Dir String -> [List-of Path]
;; returns a [List-of Path] with each Path
;; leading to file in dir, else returns null

(define (find-all dir file)
  (local (;; [List-of File] -> [List-of Path]
          (define (paths-from-files files)
            (map (lambda (file-in-files) (list (dir-name dir) file-in-files))
                 (filter (lambda (f) (string=? f file)) (map file-name files))))
          ;; [List-of Dir] -> [List-of Path]
          (define (paths-from-dirs dirs)
            (map (lambda (sub-path) (cons (dir-name dir) sub-path))
                 (foldr append
                        '()
                        (map (curryr find-all file)
                             (filter (curryr found? file) dirs))))))
    (append (paths-from-files (dir-files dir))
            (paths-from-dirs (dir-dirs dir)))))

(check-expect (find-all Code "read!") '())
(check-expect (find-all Docs "read!") '(("Docs" "read!")))
(check-expect (find-all Text "read!") '())
(check-expect (find-all Libs "read!") '(("Libs" "Docs" "read!")))
(check-expect (find-all TS "read!")
              '(("TS" "read!")
                ("TS" "Libs" "Docs" "read!")))
(check-expect (find-all TS-TS "read!")
              '(("TS-TS" "read!")
                ("TS-TS" "TS" "read!")
                ("TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TS" "TS" "read!")
                ("TS-TS" "TS" "Libs" "Docs" "read!")))
(check-expect (find-all TS-TSTS-TS-TSTS "read!")
              '(("TS-TSTS-TS-TSTS" "read!")
                ("TS-TSTS-TS-TSTS" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "read!")
                ("TS-TSTS-TS-TSTS" "TS-TS" "TS" "Libs" "Docs" "read!")))

(test)
