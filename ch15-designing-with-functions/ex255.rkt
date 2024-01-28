#lang racket
(require test-engine/racket-tests)

;; [List-of Number] [Number -> Number] -> [List-of Number]
;; applies function f to all elements of a list of numbers;
;; returns a new list of numbers.

(check-expect (map-num '() add1) '())
(check-expect (map-num (list 1) add1) (list 2))
(check-expect (map-num (list 1 2) add1) (list 2 3))
(check-expect (map-num (list 1 2 3) add1) (list 2 3 4))
(check-expect (map-num (list 1 2 3) sub1) (list 0 1 2))

(define (map-num l-num f)
  (cond [(empty? l-num) '()]
        [else (cons (f (car l-num)) (map-num (cdr l-num) f))]))

;; [List-of String] [String -> String] -> [List-of String]
;; applies function f to all elements of a list of string;
;; returns a new list of numbers.

(check-expect (map-str '() string-length) '())
(check-expect (map-str (list "a") string-upcase) (list "A"))
(check-expect (map-str (list "a" "ball") string-upcase) (list "A" "BALL"))
(check-expect (map-str (list "a" "ball" "cat") string-upcase) (list "A" "BALL" "CAT"))
(check-expect (map-str (list "A" "BALL" "CAT") string-downcase) (list "a" "ball" "cat"))

(define (map-str l-str f)
  (cond [(empty? l-str) '()]
        [else (cons (f (car l-str)) (map-str (cdr l-str) f))]))

;; [X Y] [List-of X] [X -> Y] -> [List-of Y]
;; applies function f to all elements of a list;
;; returns a new list.

(check-expect (map1 (list 1 2 3) add1) (map-num (list 1 2 3) add1))
(check-expect (map1 (list 1 2 3) sub1) (map-num (list 1 2 3) sub1))
(check-expect (map1 (list "a" "ball" "cat") string-upcase) (map-str (list "a" "ball" "cat") string-upcase))
(check-expect (map1 (list "A" "BALL" "CAT") string-downcase) (map-str (list "A" "BALL" "CAT") string-downcase))

;; why use [X Y] instead of [X] in the signature? because:
(check-expect (map1 (list "a" "ball" "cat") string-length) (map-str (list "a" "ball" "cat") string-length))
;; produces a list of numbers instead of a list of strings --
;; in other words, [List-of X] -> [List-of Y].

(define (map1 l f)
  (cond [(empty? l) '()]
        [else (cons (f (car l)) (map-str (cdr l) f))]))

(test)
