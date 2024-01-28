#lang racket
(require test-engine/racket-tests)

;; [List-of X] [List-of X] -> [List-of [List-of X]]
;; produces pairs of all items from list lx and ly

(check-expect (cross '() '()) '())
(check-expect (cross '(1) '("a")) '((1 "a")))
(check-expect (cross '(1 2) '("a" "b")) '((1 "a") (1 "b") (2 "a") (2 "b")))
(check-expect (cross '(1 2) '("a" "b" "c")) '((1 "a") (1 "b") (1 "c") (2 "a") (2 "b") (2 "c")))
(check-satisfied (cross '("a" "b" "c") '(1 2)) (lambda (l) (= (length l) 6)))

(define (cross l-x l-y)
  (for*/list ([x l-x]
              [y l-y])
    (list x y)))

;; [List-of X] [List-of X] -> [List-of [List-of X]]
;; produces pairs of all items from list lx and ly

(check-expect (cross.func '() '()) (cross '() '()))
(check-expect (cross.func '(1) '("a")) (cross '(1) '("a")))
(check-expect (cross.func '(1 2) '("a" "b")) (cross '(1 2) '("a" "b")))
(check-expect (cross.func '(1 2) '("a" "b" "c")) (cross '(1 2) '("a" "b" "c")))
(check-expect (cross.func '("a" "b" "c") '(1 2)) (cross '("a" "b" "c") '(1 2)))

(define (cross.func l-x l-y)
  (foldr
   (lambda (x l-occurring)
     (append (map (lambda (y) (list x y)) l-y)
             l-occurring))
   '()
   l-x))

(test)
