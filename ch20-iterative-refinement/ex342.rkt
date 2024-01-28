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

(define htdp-ch1 (create-dir "/home/objekt0riented/Documents/htdp/ch1-arithmetic"))
(define htdp-ch2 (create-dir "/home/objekt0riented/Documents/htdp/ch2-functions-and-programs"))
(define htdp-ch3 (create-dir "/home/objekt0riented/Documents/htdp/ch3-how-to-design-programs"))
(define htdp-ch12 (create-dir "/home/objekt0riented/Documents/htdp/ch12-projects-lists"))

;; a Path is a [List-of String]
;; i.e. directions into a directory tree

;; Dir String -> Boolean
;; checks whether file figures in dir

(define (find? dir file)
  (or (ormap (lambda (f) (string=? (file-name f) file)) (dir-files dir))
      ;; S8A's version:
      ;; (not (false? (member file (map file-name (dir-files dir)))))
      (ormap (lambda (d) (find? d file)) (dir-dirs dir))))

(check-expect (find? Code "read!") #f)
(check-expect (find? Docs "read!") #t)
(check-expect (find? Text "read!") #f)
(check-expect (find? Libs "read!") #t)
(check-expect (find? TS "read!") #t)

;; Dir String -> [Maybe Path]
;; produces a path to file if it exists within a dir,
;; otherwise produces false

(define (find.v1 dir file)
  (if (false? (find? dir file))
      #f
      (local (;; [List-of File] [List-of Dir] -> Path
              (define (find-in files dirs)
                (if (in-files? files)
                    (list file)
                    (find-in-dirs dirs)))
              ;; [List-of File] -> Boolean
              (define (in-files? files)
                (not (false? (member file (map file-name files)))))
              ;; [List-of Dir] -> Path
              (define (find-in-dirs dirs)
                (if (find? (car dirs) file)
                    (find.v1 (car dirs) file)
                    (find-in-dirs (cdr dirs)))))
        (append (list (dir-name dir))
                (find-in (dir-files dir) (dir-dirs dir))))))

(check-expect (find.v1 Code "read!") #f)
(check-expect (find.v1 Docs "read!") '("Docs" "read!"))
(check-expect (find.v1 Text "read!") #f)
(check-expect (find.v1 Libs "read!") '("Libs" "Docs" "read!"))
(check-expect (find.v1 TS "read!") '("TS" "read!"))

;; Dir String -> [Maybe Path]
;; produces a path to file if it exists within a dir,
;; otherwise produces false

(define (find.v2 dir file)
  (if (false? (find? dir file))
      #f
      (local (;; [List-of Dir] -> Path
              (define (find-in-dirs dirs)
                (local (;; [List-of Dir] -> Path
                        (define (trace-path found-in-dirs)
                          (foldr (lambda (d path)
                                   (append (find.v2 d file) path))
                                 '()
                                 found-in-dirs)))
                  (if (ormap (curryr find? file) dirs)
                      (trace-path (filter (curryr find? file) dirs))
                      (list file)))))
        (append (list (dir-name dir))
                (find-in-dirs (dir-dirs dir))))))

(check-expect (find.v2 Code "read!") #f)
(check-expect (find.v2 Docs "read!") '("Docs" "read!"))
(check-expect (find.v2 Text "read!") #f)
(check-expect (find.v2 Libs "read!") '("Libs" "Docs" "read!"))
(check-expect (find.v2 TS "read!") '("TS" "Libs" "Docs" "read!"))

;; Dir String -> [Maybe [List-of Path]]
;; produces the list of all paths that lead to file from dir

(define (find-all dir file)
  (if (false? (find? dir file))
      #f
    (local (;; [List-of File] -> Boolean
            (define (in-files? files)
              (not (false? (member file (map file-name files)))))
            ;; [List-of Dir] -> [List-of Path]
            (define (find-in-dirs dirs)
              (map (lambda (path) (append (list (dir-name dir)) path))
                   (foldr (lambda (d l-path) (append (find-all d file) l-path))
                          '()
                          (filter (curryr find? file) dirs)))))
      (if (in-files? (dir-files dir))
          (cons (list (dir-name dir) file)
                (find-in-dirs (dir-dirs dir)))
          (find-in-dirs (dir-dirs dir))))))

(check-expect (find-all TS "read") #f)
(check-expect (find-all TS "read!")
              '(("TS" "read!")
                ("TS" "Libs" "Docs" "read!")))

(test)
