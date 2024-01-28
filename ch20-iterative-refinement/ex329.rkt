#lang racket
(require test-engine/racket-tests)

;; a File-Type is one of:
;; - File
;; - Directory

(define-struct file [name size] #:transparent)
;; a File is a structure:
;;   (make-file String Number)
;; describes the name and size of a file.
(define hang (make-file 'hang 8))
(define draw (make-file 'draw 2))
(define read!-19 (make-file 'read! 19))
(define part1 (make-file 'part1 99))
(define part2 (make-file 'part2 52))
(define part3 (make-file 'part3 17))
(define read!-10 (make-file 'read! 10))

;; a Directory is one of:
;; - '()
;; - (cons File-Type Directory)
(define Code `(,hang ,draw))
(define Docs `(,read!-19))
(define Libs `(,Code ,Docs))
(define Text `(,part1 ,part2 ,part3))
(define TS `(,Text ,read!-10 ,Libs))

;; [1] -------------------------------------
;; File-Type Symbol -> Number
;; returns the frequency at which a File with a
;; file-name that matches the given sym occurs
;; within a Directory

(define (count-file f-type sym)
  (match f-type
    [(? file?) (if (symbol=? (file-name f-type) sym) 1 0)]
    [else (foldr + 0 (map (curryr count-file sym) f-type))]))

(check-expect (count-file TS 'read!) 2)
(check-expect (count-file TS 'read) 0)
(check-expect (count-file Docs 'read!) 1)

;; [2] -------------------------------------

#|

q: can you describe the path from the root directory to the occurrences?

a: for the first occurrence of read!: /TS
   for the second occurrence: /TS/Libs/Docs

|#

;; possible auxiliary function?
;; File-Type File -> Number
;; checks whether a File exists in a File-Type

(define (contains-file? f-type f)
  (match f-type
    [(? file?) (equal? f-type f)]
    [else (ormap (curryr contains-file? f) f-type)]))

(check-expect (contains-file? read!-10 read!-10) #t)
(check-expect (contains-file? TS read!-10) #t)
(check-expect (contains-file? Docs read!-10) #f)
(check-expect (contains-file? Docs read!-19) #t)

;; [3] -------------------------------------
;; File-Type -> Number
;; returns the total size of all Files in a File-Type

(define (total-size/files f-type)
  (match f-type
    [(? file?) (file-size f-type)]
    [else (for/sum ([i f-type]) (total-size/files i))]))

(check-expect (total-size/files '()) 0)
(check-expect (total-size/files TS) (+ 10 99 52 17 19 8 2))
(check-expect (total-size/files Libs) (+ 8 2 19))
(check-expect (total-size/files read!-10) 10)

;; [4] -------------------------------------
;; File-Type -> Number
;; returns the total size of all Files and Directories
;; (each being of size 1) in a File-Type

(define (total-size f-type)
  (local ((define directory-size 1))
    (match f-type
      [(? file?) (file-size f-type)]
      [else (foldr + directory-size (map total-size f-type))])))

(check-expect (total-size '()) 1) ;; size of empty directory
(check-expect (total-size TS) (+ 1 1 99 52 17 10 1 1 8 2 1 19))
(check-expect (total-size Libs) (+ 1 1 8 2 1 19))
(check-expect (total-size read!-10) 10)

;; [5] -------------------------------------
;; File-Type -> Number
;; returns the total level of Directories in a File-Type

(define (directory-depth f-type)
  (match f-type
    [(? file?) 1]
    [else (foldr max 1 (map (lambda (x) (add1 (directory-depth x))) f-type))]))

(check-expect (directory-depth '()) 1) ;; depth of an empty directory
(check-expect (directory-depth TS) 4)
(check-expect (directory-depth Libs) 3)

(test)
