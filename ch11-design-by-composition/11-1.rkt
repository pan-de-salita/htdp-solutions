#lang racket
(require test-engine/racket-tests)

;; ex181

;; ex182

(check-expect
 (list 0 1 2 3 4 5)
 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))

(check-expect
 (list (list "he" 0) (list "it" 1) (list "lui" 14))
 (cons (cons "he" (cons 0 '()))
       (cons (cons "it" (cons 1 '()))
             (cons (cons "lui" (cons 14 '())) '()))))

(check-expect
 (list 1 (list 1 2) (list 1 2 3))
 (cons 1
       (cons (cons 1 (cons 2 '()))
             (cons (cons 1 (cons 2 (cons 3 '()))) '()))))

;; ex183

(check-expect
 (cons "a" (list 0 #false))
 (cons "a" (cons 0 (cons #false '()))))
(check-expect
 (cons "a" (list 0 #false))
 (list "a" 0 #false))

(check-expect
 (list (cons 1 (cons 13 '())))
 (cons (cons 1 (cons 13 '())) '()))
(check-expect
 (list (cons 1 (cons 13 '())))
 (list (list 1 13)))

(check-expect
 (cons (list 1 (list 13 '())) '())
 (cons (cons 1
             (cons (cons 13
                         (cons '() '())) '())) '()))
(check-expect
 (cons (list 1 (list 13 '())) '())
 (list (list 1 (list 13 '()))))

(check-expect
 (list '() '() (cons 1 '()))
 (cons '()
       (cons '()
             (cons (cons 1 '()) '()))))
(check-expect
 (list '() '() (cons 1 '()))
 (list '() '() (list 1)))

(check-expect
 (cons "a" (cons (list 1) (list #false '())))
 (cons "a"
       (cons (cons 1 '())
             (cons #false
                   (cons '() '())))))
(check-expect
 (cons "a" (cons (list 1) (list #false '())))
 (list "a" (list 1) #false '()))

;; ex184

(check-expect
 (list (string=? "a" "b") #false)
 (cons #false (cons #false '())))
(check-expect
 (list (string=? "a" "b") #false)
 (list #false #false))

(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (cons 30 (cons 200 (cons 1/2 '()))))
(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (list 30 200 1/2))

(check-expect
 (list "dana" "jane" "mary" "laura")
 (cons "dana" (cons "jane" (cons "mary" (cons "laura" '())))))
(check-expect
 (list "dana" "jane" "mary" "laura")
 (list "dana" "jane" "mary" "laura"))

;; ex185

(check-expect (first (list 1 2 3)) (car (list 1 2 3)))

(check-expect (rest (list 1 2 3)) (cdr (list 1 2 3)))

(check-expect (second (list 1 2 3)) (cadr (list 1 2 3)))

(test)
