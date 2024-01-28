#lang racket
(require test-engine/racket-tests)

;; 1.
(check-expect
 (list (string=? "a" "b") #false)
 (list #false #false))
(check-expect
 (list (string=? "a" "b") #false)
 (cons #f (cons #f '())))

;; 2.
(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (list 30 200 1/2))
(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (cons 30 (cons 200 (cons 1/2 '()))))

;; 3.
(check-expect
 (list "dana" "jane" "mary" "laura")
 (list "dana" "jane" "mary" "laura"))
(check-expect
 (list "dana" "jane" "mary" "laura")
 (cons "dana" (cons "jane" (cons "mary" (cons "laura" '())))))

(test)
