#lang racket
(require test-engine/racket-tests)

;; a File.v1 is a String
(define-values
  (read!-10.v1 part1.v1 part2.v1 part3.v1 read!-19.v1 hang.v1 draw.v1)
  (values "read! (10)" "part1 (99)" "part2 (52)" "part3 (17)" "read! (19)" "hang (8)" "draw (2)"))

;; a Dir.v1 (short for Directory) is one of:
;; - '()
;; - (cons File.v1 Dir.v1)
;; - (cons Dir.v1 Dir.v1)
(define Code.v1 `(,hang.v1 ,draw.v1))
(define Docs.v1 `(,read!-19.v1))
(define Text.v1 `(,part1.v1 ,part2.v1 ,part3.v1))
(define Libs.v1 `(,Code.v1 ,Docs.v1))
(define TS.v1 `(,Text.v1 ,read!-10.v1 ,Libs.v1))

;; Dir.v1 -> Number
;; returns the amount of files a dir.v1 contains

(define (how-many dir.v1)
  (cond
    [(null? dir.v1) 0]
    [(string? (car dir.v1)) (add1 (how-many (cdr dir.v1)))]
    [(cons? (car dir.v1)) (+ (how-many (car dir.v1))
                             (how-many (cdr dir.v1)))]))

(check-expect (how-many Code.v1) 2)
(check-expect (how-many Docs.v1) 1)
(check-expect (how-many Text.v1) 3)
(check-expect (how-many Libs.v1) (+ (how-many Code.v1) (how-many Docs.v1)))
(check-expect (how-many TS.v1) (+ (how-many Text.v1) 1 (how-many Libs.v1)))

;; Dir.v1 -> Number
;; like how-many, returns the amount of files a dir.v1 contains

(define (how-many-simplified dir.v1)
  (local (;; [File.v1 or Dir.v2] -> Number
          (define (count-files dir)
            (if (string? dir)
                1
                (how-many-simplified dir))))
    (foldr + 0 (map count-files dir.v1))))

(check-expect (how-many-simplified Code.v1) (how-many Code.v1))
(check-expect (how-many-simplified Docs.v1) (how-many Docs.v1))
(check-expect (how-many-simplified Text.v1) (how-many Text.v1))
(check-expect (how-many-simplified Libs.v1) (how-many Libs.v1))
(check-expect (how-many-simplified TS.v1) (how-many TS.v1))

(test)
