#lang racket
(require test-engine/racket-tests)

;; 1.
(check-expect
 (cons "a" (list 0 #false))
 (list "a" 0 #false))

(check-expect
 (cons "a" (list 0 #false))
 (cons "a" (cons 0 (cons #false '()))))

;; 2.
(check-expect
 (list (cons 1 (cons 13 '())))
 (list (list 1 13)))

(check-expect
 (list (cons 1 (cons 13 '())))
 (cons (cons 1 (cons 13 '())) '()))

;; 3.
(check-expect
 (cons (list 1 (list 13 '())) '())
 (list (list 1 (list 13 '()))))

(check-expect
 (cons (list 1 (list 13 '())) '())
 (cons (cons 1 (cons (cons 13 (cons '() '())) '())) '()))

;; 4.
(check-expect
 (list '() '() (cons 1 '()))
 (list '() '() (list 1)))

(check-expect
 (list '() '() (cons 1 '()))
 (cons '() (cons '() (cons (cons 1 '()) '()))))

;; 5.
(check-expect
 (cons "a" (cons (list 1) (list #false '())))
 (list "a" (list 1) #false '()))

(check-expect
 (cons "a" (cons (list 1) (list #false '())))
 (cons "a" (cons (cons 1 '()) (cons #false (cons '() '())))))

 (test)
