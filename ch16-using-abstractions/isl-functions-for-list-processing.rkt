#lang racket
(require test-engine/racket-tests)

;; [X] N [N -> X] -> [List-of X]
;; constructs a list by appying f to 0, 1, ... , (sub1 n)
;; (build-list n f) == (list (f 0) ... (f (- n 1)))

(check-expect (build-list 3 add1) (list 1 2 3))

;; [X] [X -> Boolean] [List-of X] -> [List-of X]
;; produces a list from those items on lx for which p holds

(check-expect (filter even? (list 1 2 3 4 5 6)) (list 2 4 6))

;; [X] [List-of X] [X X -> Boolean] -> [List-of X]
;; produces a version of lx that is sorted according to cmp

(check-expect (sort (list 3 2 1 4 5) >) (list 5 4 3 2 1))

;; [X Y] [X -> Y] [List-of X] -> [List-of Y]
;; constructs a list by applying f to each item on lx
;; (map f (list x-1 ... x-n)) == (list (f x-1) ... (f x-n))

(check-expect (map sqr (list 1 2 3)) (list 1 4 9))

;; [X] [X -> Boolean] [List-of X] -> Boolean
;; determines whether p holds for every item on lx
;; (andmap p (list x-1 ... x-n)) == (and (p x-1) ... (p x-n))

(check-expect (andmap even? (list 1 2 3)) #f)

;; [X] [X -> Boolean] [List-of X] -> Boolean
;; determines whether p holds for at least one item on lx
;; (ormap p (list x-1 .. x-n)) == (or (p x-1) ... (p x-n))

(check-expect (ormap even? (list 1 2 3)) #t)

;;;; reductive functions:

;; [X Y] [X Y -> Y] Y [List-of X] -> Y
;; applies f from right to left to each item in lx and b
;; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))

#|

(foldr + 0 (list 1 2 3 4 5))
== (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 0)))))
== (+ 1 (+ 2 (+ 3 (+ 4 5))))
== (+ 1 (+ 2 (+ 3 9)))
== (+ 1 (+ 2 12))
== (+ 1 14)
== 15

|#


;; [X Y] [X Y -> Y] Y [List-of X] -> Y
;; applies f from left to right to each item in lx and b
;; (foldl f b (list x-1 ... x-n)) == (f x-n ... (f x-1 b))

#|

(foldl + 0 (list 1 2 3 4 5))
== (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 0)))))
== (+ 5 (+ 4 (+ 3 (+ 2 1))))
== (+ 5 (+ 4 (+ 3 3)))
== (+ 5 (+ 4 6))
== (+ 5 10)
== 15

|#

(check-expect (foldl + 0 (list 1 2 3 4 5)) (foldr + 0 (list 1 2 3 4 5)))

(test)
